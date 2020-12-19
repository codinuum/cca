#!/usr/bin/env python3

'''
  A driver script for CCA container image

  Copyright 2020 Codinuum Software Lab <https://codinuum.com>

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
from datetime import datetime, timedelta
from subprocess import Popen, call
from threading import Thread
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

IMAGE_NAME = 'codinuum/cca'

#

CCA_HOME = '/opt/cca'
CCA_VAR = '/var/lib/cca'
CCA_LOG_DIR = '/var/log/cca'

CCA_SOURCE_DIR = CCA_VAR+'/source'
CCA_CACHE_DIR = CCA_VAR+'/cache'

CONTAINER_CMD = 'docker'

BUFSIZE = 0 # unbuffered

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

    TZ = '%s%s%s' % (tzname, SIGN, offset)

###

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

def run_diffast(original, modified, cache=DEFAULT_CACHE_DIR, clear_cache=False, view=False,
                dry_run=False, devel=False, image=IMAGE_NAME, verbose=False):

    if dry_run:
        verbose = True

    original = os.path.abspath(original)
    modified = os.path.abspath(modified)
    cache = os.path.abspath(cache)

    if not dry_run:
        ensure_dir(cache)

    cca_cmd_path = '%s/bin/%s.opt' % (CCA_HOME, 'diffast')
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

    run_cmd = '%s run' % CONTAINER_CMD
    run_cmd += ' --rm'
    run_cmd += ' -t'

    if TZ:
        run_cmd += ' -e "TZ=%s"' % TZ

    run_cmd += ' %s' % vol_opt
    run_cmd += ' %s %s' % (get_image_name(image, devel=devel), cca_cmd)

    if verbose:
        print(run_cmd)

    if not dry_run:
        try:
            rc = call(run_cmd, bufsize=BUFSIZE, shell=True, universal_newlines=True)

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
                    rc = call(view_cmd, shell=True)
                else:
                    print('DiffViewer not found. See diffviewer/README.md.')

        except (KeyboardInterrupt, SystemExit):
            print('Interrupted.')

        except OSError as e:
            print('Execution failed: {}'.format(e))


def update(args):
    cmd = '%s pull %s' % (CONTAINER_CMD, get_image_name(args.image, devel=args.devel))
    if args.verbose or args.dry_run:
        print(cmd)
    if not args.dry_run:
        try:
            call(cmd, shell=True)
        except OSError as e:
            print('Execution failed: {}'.format(e))

def diffast(args):
    run_diffast(args.original, args.modified, cache=args.cache, clear_cache=args.force, view=args.view,
                dry_run=args.dry_run, devel=args.devel, image=args.image, verbose=args.verbose)



def main():
    parser = ArgumentParser(description='CCA driver',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('-n', '--dry-run', dest='dry_run', action='store_true',
                        help='only print container commands')

    parser.add_argument('-i', '--image', dest='image', type=str, metavar='IMAGE', default=IMAGE_NAME,
                        help='specify container image')

    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='be verbose')

    parser.add_argument('-x', '--experimental', dest='devel', action='store_true',
                        help='use experimental image')

    subparsers = parser.add_subparsers(title='subcommands')

    parser_update = subparsers.add_parser('update',
                                          description='Update docker image of CCA',
                                          formatter_class=ArgumentDefaultsHelpFormatter)

    parser_update.set_defaults(func=update)

    parser_diffast = subparsers.add_parser('diffast',
                                           description='Compare two programs',
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


    args = parser.parse_args()

    try:
        args.func(args)
    except:
        #raise
        parser.print_help()


if __name__ == '__main__':
    main()
