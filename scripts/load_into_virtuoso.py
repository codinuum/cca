#!/usr/bin/env python3


'''
  A fact loader

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

import os.path
import sys

import pathsetup
from pathsetup import CCA_HOME, LOG_DIR


import virtuoso
from virtuoso import (VTMP_DIR,
                      DB_DIR,
                      GRAPH_URI_BASE,
                      DEFAULT_PORT,
                      DEFAULT_MAX_FILES,
                      VIRTUOSO_PW)


def load(proj_id, db_dir, fact_dir, exts, port=DEFAULT_PORT, pw=VIRTUOSO_PW):
    graph_uri = GRAPH_URI_BASE+proj_id

    loader = virtuoso.Loader(db_dir, daemonize=False, pw=pw, port=port)

    rc = loader.disable_checkpoint()
    if rc != 0:
        loader.message('starting server...')
        loader.start_server()
        loader.disable_checkpoint()

    rc = loader.load(graph_uri, fact_dir, exts, nprocs=1)

    return rc


if __name__ == '__main__':
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    default_fdir = os.path.join(VTMP_DIR, '<PROJ_ID>')

    parser = ArgumentParser(description='load fact into virtuoso',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('proj_id', metavar='PROJ_ID', type=str, help='project id')
    parser.add_argument('exts', metavar='EXT', type=str, nargs='+',
                        help='fact extensions (ex. .ttl)')

    parser.add_argument('--dbdir', dest='dbdir', default=DB_DIR,
                        metavar='DIR', type=str, help='database directory')

    parser.add_argument('--factdir', dest='fdir', metavar='DIR', default=default_fdir, type=str,
                        help='fact directory')
    
    parser.add_argument('--daemon', dest='daemon', action='store_true', help='run as an daemon')

    parser.add_argument('--resume', dest='resume', action='store_true', help='resume loading')

    parser.add_argument('-l', '--log', dest='logdir', metavar='DIR', default=LOG_DIR, type=str,
                        help='log directory')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('-p', '--nprocs', dest='nprocs', type=int, default=1, metavar='N',
                        help='run N processes')

    parser.add_argument('-n', '--nfiles', dest='nfiles', type=int, default=DEFAULT_MAX_FILES,
                        metavar='N', help='N files are loaded per load')

    args = parser.parse_args()


    graph_uri = GRAPH_URI_BASE+args.proj_id
    fdir = os.path.join(VTMP_DIR, args.proj_id)
    if args.fdir != default_fdir:
        fdir = args.fdir


    def doit():
        loader = virtuoso.Loader(args.dbdir, daemonize=args.daemon)

        if args.debug:
            loader.set_debug_flag()

        rc = loader.disable_checkpoint()
        if rc != 0:
            loader.message('starting server...')
            loader.start_server()
            loader.disable_checkpoint()

        loader.load(graph_uri, 
                    fdir, 
                    args.exts, 
                    nprocs=args.nprocs, 
                    maxfiles=args.nfiles, 
                    resume=args.resume)

    if args.daemon:
        log_dir = args.logdir
        from daemon import DaemonContext
        cn = os.path.splitext(os.path.basename(sys.argv[0]))[0]
        fname_fmt = '%s.%s.%%s.log' % (cn, args.proj_id)
        log_stdout = os.path.join(log_dir, (fname_fmt % 'stdout'))
        log_stderr = os.path.join(log_dir, (fname_fmt % 'stderr'))
        context = DaemonContext(
            working_directory=CCA_HOME,
            umask=0o022,
            stdout=open(log_stdout, 'a'),
            stderr=open(log_stderr, 'a')
            )
        with context:
            doit()

    else:
        doit()
