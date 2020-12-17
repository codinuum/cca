#!/usr/bin/env python3


'''
  cca_command.py

  Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

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

import sys
import os
import multiprocessing
from daemon import DaemonContext
import tempfile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, Action
import logging

import pathsetup
import proc
from common import setup_logger

logger = logging.getLogger()

time_cmd = '/usr/bin/time'
if sys.platform.startswith('darwin'):
    time_cmd = '/opt/local/bin/gtime'

class store_subarg(Action):
    def __call__(self, parser, namespace, values, option_string):
        setattr(namespace, self.dest, getattr(namespace, self.dest)+values[0])


def sub(args, PHASE, PROJ, WDIR_BASE, NPROCS):
    logger.info('PHASE:"{}"'.format(PHASE))
    logger.info('PROJ:"{}"'.format(PROJ))
    logger.info('WDIR_BASE:"{}"'.format(WDIR_BASE))
    logger.info('NPROCS:"{}"'.format(NPROCS))
    logger.info('sub args:"{}"'.format(args.sargs))

    SUB_CMD_NAME = 'cca_{}.py'.format(PHASE)

    DIST_DIR = os.path.dirname(sys.argv[0])

    SUB_CMD_PATH = os.path.join(DIST_DIR, SUB_CMD_NAME)

    PROJ_ = PROJ.replace(os.path.sep, '_').replace('.', '_')

    WDIR = os.path.join(WDIR_BASE, 'work.'+PHASE, PROJ_)
    if not os.path.exists(WDIR):
        os.makedirs(WDIR)

    cmd_fmt = time_cmd + ' -o %(log)s %(subcmd)s'

    log_base = os.path.join(WDIR,'time.{}-{}'.format(PHASE, PROJ_))

    gencmd = cmd_fmt % { 'log'    : log_base + '.generate',
                         'subcmd' : '{} {} -c generate {} {}'.format(SUB_CMD_PATH,
                                                                     args.sargs,
                                                                     PROJ,
                                                                     WDIR),
                         }

    sub_args = '{} -b {}'.format(PROJ, WDIR_BASE)
    if args.sargs != '':
        sub_args += ' '+args.sargs

    sub_args = '-a "{}"'.format(sub_args)

    run_workers_cmd_path = os.path.join(DIST_DIR, 'run_workers.py')

    workcmd = cmd_fmt % { 'log'   : log_base + '.work',
                          'subcmd' : '{} -n {} -c {} {} {}'.format(run_workers_cmd_path, 
                                                                   NPROCS, 
                                                                   SUB_CMD_NAME, 
                                                                   sub_args, 
                                                                   WDIR),
                          }

    collcmd = cmd_fmt % { 'log'   : log_base + '.collect',
                          'subcmd' : '{} {} -c collect -b {} {} {}'.format(SUB_CMD_PATH, 
                                                                           PROJ, 
                                                                           WDIR_BASE,
                                                                           args.sargs,
                                                                           WDIR),
                          }

    cmds = [gencmd, workcmd]

    if args.collect_only:
        cmds = []

    if args.no_collect:
        pass

    else:
        cmds.append(collcmd)

    stat = 0

    for c in cmds:
        logger.info('cmd=[{}]'.format(c))
        rc = proc.system(c)
        if rc != 0:
            stat = rc
            logger.warning('failed to execute (code={}): "{}"'.format(rc, c))

    return stat


def main():
    argparser = ArgumentParser(description='CCA command driver',
                               formatter_class=ArgumentDefaultsHelpFormatter)

    argparser.add_argument('phase', type=str, metavar='PHASE')
    argparser.add_argument('proj', type=str, metavar='PROJ')

    argparser.add_argument('--daemon', action='store_true', dest='daemon',
                           default=False, help='run as daemon')

    argparser.add_argument('--collect-only', action='store_true', dest='collect_only',
                           default=False, help='only collect results')

    argparser.add_argument('--no-collect', action='store_true', dest='no_collect',
                           default=False, help='disable result collection')

    argparser.add_argument('-p', '--nprocs', type=int, dest='nprocs',
                           default=0, metavar='N',
                           help='set number of processors')

    argparser.add_argument('-a', '--arg', action=store_subarg, nargs=1, type=str,
                           dest='sargs', default='', metavar='ARG',
                           help='set arg for sub command')

    argparser.add_argument('-b', '--basedir', type=str, dest='basedir',
                           default='.', metavar='DIR', help='set base dir to DIR')

    args = argparser.parse_args()

    setup_logger(logger, logging.INFO)

    PHASE     = args.phase
    PROJ      = args.proj
    WDIR_BASE = args.basedir
    NPROCS    = multiprocessing.cpu_count() / 2 # assuming HT
    if args.nprocs != 0:
        NPROCS = args.nprocs

    stat = 0

    if args.daemon:
        log_stdout = os.path.join(WDIR_BASE, 'cca.'+PROJ+'.stdout.log')
        log_stderr = os.path.join(WDIR_BASE, 'cca.'+PROJ+'.stderr.log')
        context = DaemonContext(
            working_directory=pathsetup.CCA_HOME,
            umask=0o022,
            stdout=open(log_stdout, 'w+'),
            stderr=open(log_stderr, 'w+')
            )

#        logger.info('pid file: {}'.format(pidfile))

        with context:
            stat = sub(args, PHASE, PROJ, WDIR_BASE, NPROCS)

    else:
        stat = sub(args, PHASE, PROJ, WDIR_BASE, NPROCS)

    if stat != 0:
        exit(stat)


if __name__ == '__main__':
    main()
