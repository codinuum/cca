#!/usr/bin/env python3

'''
  patchast.py

  Copyright 2018 Chiba Institute of Technology

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

__author__ = 'Masatomo Hashimoto <m.hashimoto@stair.center>'

import logging

from . import proc
from .diffts import patchast_cmd
from .common import setup_logger

logger = logging.getLogger()


def patchast(src_path, delta_path, quiet=True):
    cmd = patchast_cmd
    cmd += ' %s %s' % (src_path, delta_path)
    stat = proc.system(cmd, quiet=quiet)
    return stat


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='apply AST delta',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('src_path', type=str, help='source directory')
    parser.add_argument('delta_path', type=str, help='delta bundle')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='enable verbose printing')

    args = parser.parse_args()

    log_level = logging.WARNING
    if args.verbose:
        log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)

    proc.logger = logger

    patchast(args.src_path, args.delta_path, quiet=(not args.debug))


if __name__ == '__main__':
    main()
