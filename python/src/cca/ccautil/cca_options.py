#!/usr/bin/env python3


'''
  cca_options.py

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

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import os
import time
import tempfile
import logging

from .common import setup_logger

logger = logging.getLogger()


class Parser(object):
    def __init__(self):
        self.argparser = ArgumentParser(description='CCA',
                                        formatter_class=ArgumentDefaultsHelpFormatter)

        self.argparser.add_argument('proj_id', type=str)
        self.argparser.add_argument('work_dir', type=str)

        self.argparser.add_argument('-b', '--basedir', dest='basedir',
                                    default='.', metavar='DIR',
                                    help='set base dir to DIR (default=%default)')

        self.argparser.add_argument('-f', '--factbase', dest='fbase',
                                    default=None,
                                    help='set factbase to DIR', metavar='DIR')

        self.argparser.add_argument('-w', '--workerid', dest='wid',
                                    default=str(time.time()),
                                    help='set worker id to ID', metavar='ID')

        self.argparser.add_argument('-c', '--command', dest='command',
                                    choices=['generate', 'work', 'collect'],
                                    default='generate',
                                    help='execute (default=%default)')

        self.argparser.add_argument('-l', '--limit', dest='max_ntasks',
                                    default=1024,
                                    action='store', type=int, metavar='LIM',
                                    help='set limit of ntasks to LIM (default=%default)')

        self.argparser.add_argument('-d', '--debug', action='store_true',
                                    dest='debug',
                                    help='enable debug output')

        self.argparser.add_argument('-k', '--keepcache', action='store_true',
                                    dest='keep_cache', help='keep caches')

    def _get_parser(self):
        return self.argparser

    def get(self):

        args = self.argparser.parse_args()

        log_level = logging.INFO
        if args.debug:
            log_level = logging.DEBUG
        setup_logger(logger, log_level)

        if not args.command:
            self.argparser.print_help()
            exit(1)

        proj_id = None
        working_dir = None

        proj_id = args.proj_id
        try:
            working_dir = args.work_dir
        except Exception:
            if not os.path.exists(args.basedir):
                os.makedirs(args.basedir)

            d = tempfile.mkdtemp('', 'tmp', args.basedir)
            working_dir = d
            logger.warning('working dir set to "{}"'.format(working_dir))

        return (args, proj_id, working_dir, self.argparser)
