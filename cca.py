#!/usr/bin/env python3

'''
  A driver script for CCA container image

  Copyright 2021 Codinuum Software Lab <https://codinuum.com>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
'''

import os
import sys
import time
import shutil
from datetime import datetime, timedelta
from subprocess import Popen, run
from threading import Thread
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

IMAGE_NAME = 'codinuum/cca'
#IMAGE_NAME = 'ccax'

#

CCA_HOME = '/opt/cca'
CCA_VAR = '/var/lib/cca'
CCA_LOG_DIR = '/var/log/cca'

CCA_SOURCE_DIR = CCA_VAR+'/source'
CCA_CACHE_DIR = CCA_VAR+'/cache'

CCA_WORK_DIR_NAME = '__CCA__'

CONTAINER_CMD = 'docker'

TIMEOUT = 5
BUFSIZE = 0 # unbuffered
STAT_NAME = 'status'

DEFAULT_CACHE_DIR = os.path.join(os.environ['HOME'], '.cca', 'cache')

#WIN_HOST_FLAG = sys.platform.startswith('win')

### timezone

TZ = None

if time.timezone != 0:
    SIGN = '+' if time.timezone > 0 else '-'

    STDOFFSET = timedelta(seconds=-time.timezone)
    if time.daylight:
        DSTOFFSET = timedelta(seconds=-time.altzone)
    else:
        DSTOFFSET = STDOFFSET

    dt = datetime.now()
    tt = (dt.year, dt.month, dt.day,
          dt.hour, dt.minute, dt.second,
          dt.weekday(), 0, 0)
    stamp = time.mktime(tt)
    tt = time.localtime(stamp)

    isdst = tt.tm_isdst > 0

    tzname = None
    offset = 0

    if isdst:
        tzname = time.tzname[1]
        offset = DSTOFFSET
    else:
        tzname = time.tzname[0]
        offset = STDOFFSET

    TZ = '{}{}{}'.format(tzname, SIGN, offset)

###

def progress(proc, stat_path, timeout=TIMEOUT):
    stat_mtime = None

    print('\nMonitoring thread started.')

    while True:
        try:
            st = os.stat(stat_path)
            if st.st_mtime != stat_mtime and st.st_size > 0:
                with open(stat_path, 'r') as f:
                    mes = f.read()
                    print('[{}]'.format(mes))

                stat_mtime = st.st_mtime

        except OSError as e:
            pass

        if proc.poll() is not None:
            break

    proc.wait()
    if proc.returncode > 0:
        print('Execution failed: {}'.format(proc.returncode))

def ensure_dir(dpath):
    if not os.path.isdir(dpath):
        try:
            os.makedirs(dpath)
        except Exception as e:
            raise

def get_image_name(image_name, devel=False):
    suffix = ''
    if devel:
        suffix = ':devel'
    image = image_name+suffix
    return image

def run_diffast(container_cmd, original, modified, cache=DEFAULT_CACHE_DIR, clear_cache=False, view=False,
                dry_run=False, devel=False, image=IMAGE_NAME, verbose=False, debug=False):

    if dry_run:
        verbose = True

    original = os.path.abspath(original)
    modified = os.path.abspath(modified)
    cache = os.path.abspath(cache)

    if not dry_run:
        ensure_dir(cache)

    cca_cmd_path = '{}/bin/{}.opt'.format(CCA_HOME, 'diffast')
    cca_cmd = cca_cmd_path
    if clear_cache:
        cca_cmd += ' -clearcache'

    cca_cmd += ' -cache {}'.format(CCA_CACHE_DIR)

    orig_dir = os.path.dirname(original)
    mod_dir = os.path.dirname(modified)

    common_path = os.path.commonpath([orig_dir, mod_dir])

    orig_path = CCA_SOURCE_DIR+'/'+os.path.relpath(original, start=common_path)
    mod_path = CCA_SOURCE_DIR+'/'+os.path.relpath(modified, start=common_path)

    cca_cmd += ' {} {}'.format(orig_path, mod_path)

    vol_opt = '-v "{}:{}"'.format(common_path, CCA_SOURCE_DIR)
    vol_opt += ' -v "{}:{}"'.format(cache, CCA_CACHE_DIR)

    run_cmd = '{} run'.format(container_cmd)
    run_cmd += ' --rm'
    run_cmd += ' -t'

    if TZ:
        run_cmd += ' -e "TZ={}"'.format(TZ)

    run_cmd += ' {}'.format(vol_opt)
    run_cmd += ' {} {}'.format(get_image_name(image, devel=devel), cca_cmd)

    if verbose:
        print(run_cmd)

    if not dry_run:
        try:
            rc = run(run_cmd, bufsize=BUFSIZE, shell=True, universal_newlines=True)

            if view:
                app_path = os.path.join(os.path.dirname(sys.argv[0]),
                                        'diffviewer',
                                        'DiffViewer-darwin-x64',
                                        'DiffViewer.app')
                if os.path.exists(app_path):
                    cache_opt = ' --cache {}'.format(cache)
                    files_opt = ' --file0 {} --file1 {}'.format(original, modified)
                    view_cmd = 'open -n {} --args{}{}'.format(app_path, cache_opt, files_opt)
                    if verbose:
                        print(view_cmd)
                    rc = run(view_cmd, shell=True)
                else:
                    print('DiffViewer not found. See diffviewer/README.md.')

        except (KeyboardInterrupt, SystemExit):
            print('Interrupted.')

        except OSError as e:
            print('Execution failed: {}'.format(e))


def gen_work_dir_name():
    dt = datetime.now()
    ts = '{:04}{:02}{:02}{:02}{:02}{:02}'.format(dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second)
    dn = '{}{}'.format(CCA_WORK_DIR_NAME, ts)
    return dn

def update(args):
    cmd = '{} pull {}'.format(args.container_cmd, get_image_name(args.image, devel=args.devel))
    if args.verbose or args.dry_run:
        print(cmd)
    if not args.dry_run:
        try:
            run(cmd, shell=True)
        except OSError as e:
            print('Execution failed: {}'.format(e))

def diffast(args):
    run_diffast(args.container_cmd,
                args.original, args.modified, cache=args.cache, clear_cache=args.force, view=args.view,
                dry_run=args.dry_run, devel=args.devel, image=args.image, verbose=args.verbose, debug=args.debug)



def main():
    parser = ArgumentParser(description='A CCA driver',
                            add_help=False,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('-n', '--dry-run', dest='dry_run', action='store_true',
                        help='only print container commands')

    parser.add_argument('--container-command', dest='container_cmd', metavar='CMD',
                        help='specify container command', default=CONTAINER_CMD)

    parser.add_argument('-i', '--image', dest='image', type=str, metavar='IMAGE', default=IMAGE_NAME,
                        help='specify container image')

    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='enable verbose printing')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('-x', '--experimental', dest='devel', action='store_true',
                        help='use experimental image')

    p = ArgumentParser(add_help=True)

    subparsers = p.add_subparsers(title='subcommands')

    # Docker image update

    parser_update = subparsers.add_parser('update',
                                          description='Update docker image of CCA',
                                          parents=[parser],
                                          formatter_class=ArgumentDefaultsHelpFormatter)

    parser_update.set_defaults(func=update)

    # Diff/AST

    parser_diffast = subparsers.add_parser('diffast',
                                           description='Compare two programs',
                                           parents=[parser],
                                           formatter_class=ArgumentDefaultsHelpFormatter)

    parser_diffast.add_argument('original', type=str, metavar='ORIGINAL', help='original source file')

    parser_diffast.add_argument('modified', type=str, metavar='MODIFIED', help='modified source file')

    parser_diffast.add_argument('--view', dest='view', action='store_true',
                                help='launch DiffViewer after comparison')

    parser_diffast.add_argument('-f', '--force', dest='force', action='store_true',
                                help='force comparison (overwrite cache)')

    parser_diffast.add_argument('-c', '--cache', dest='cache', default=DEFAULT_CACHE_DIR,
                                 metavar='DIR', type=str, help='result cache directory')

    parser_diffast.set_defaults(func=diffast)

    #

    args = p.parse_args()

    try:
        args.func(args)
    except:
        #raise
        p.print_help()


if __name__ == '__main__':
    main()
