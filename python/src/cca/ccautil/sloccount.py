#!/usr/bin/env python3

'''
  A sloccount driver: sloccount.py

  Copyright 2019-2024 Chiba Institute of Technology

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

import os
import re
import tempfile
import logging

from subprocess import Popen, PIPE

logger = logging.getLogger()

###

SLOCCOUNT = 'sloccount'

HEAD = 'Totals grouped by language'

PAT = re.compile(r'\b(?P<lang>\w+)\s*:\s*(?P<sloc>\d+)', re.I)

LANG_TBL = {
    'c': ['ansic'],
    'fortran': ['fortran', 'f90'],
}

###


def escape(s):
    if '$' in s:
        s = s.replace('$', '\\$')
    return s


class PopenContext(object):
    def __init__(self, cmd, rc_check=True):
        self.cmd = cmd
        self.rc_check = rc_check

    def __enter__(self):
        self._po = Popen(self.cmd,
                         shell=True,
                         stdout=PIPE,
                         stderr=PIPE,
                         close_fds=True,
                         universal_newlines=True)
        return self._po

    def __exit__(self, *exc_info):
        (exc, v, tr) = exc_info

        if exc == OSError:
            logger.error(f'execution failed: {v}')
            return True

        elif exc is None:
            rc = self._po.returncode
            if rc and self.rc_check:
                if rc != 0:
                    logger.warning(f'"{self.cmd}": terminated abnormally (exitcode={rc})')
            return True

        else:
            return False


def get_langs(lang):
    ll = [lang]
    try:
        ll = LANG_TBL[lang]
    except KeyError:
        pass
    return ll


def sloccount(path, datadir=None):
    opts = ' --follow --autogen'
    if datadir:
        opts += f' --datadir {datadir}'

    path = escape(path)

    cmd = f'{SLOCCOUNT}{opts} "{path}"'
    logger.debug(f'cmd="{cmd}"')

    c = PopenContext(cmd)
    total_sloc = 0
    sloc_tbl = {}
    with c as p:
        (o, e) = p.communicate()
        flag = False
        for _line in o.split('\n'):
            line = _line.strip()
            logger.debug(f'line="{line}"')

            if flag:
                m = PAT.search(line)
                if m:
                    try:
                        lang = m.group('lang')
                        logger.debug(f'LANG="{lang}"')
                        sloc = int(m.group('sloc'))
                        logger.debug(f'SLOC="{sloc}"')
                        total_sloc += sloc

                        try:
                            sloc_tbl[lang] += sloc
                        except KeyError:
                            sloc_tbl[lang] = sloc

                    except Exception as e:
                        logger.warning(f'{e}')
                else:
                    flag = False
                    logger.debug('END')

            if line.startswith(HEAD):
                flag = True
                logger.debug('BEGIN')

    return {'tbl': sloc_tbl, 'total': total_sloc}


def sloccount_str(content):
    (fd, tmp) = tempfile.mkstemp()
    os.close(fd)

    f = open(tmp, 'w')
    f.write(content)
    f.close()

    sloc = sloccount(tmp)

    os.unlink(tmp)

    return sloc


def sloccount_for_lang(lang, path, datadir=None):
    ll = get_langs(lang)
    logger.debug(f'lang="{lang}" ({",".join(ll)})')
    r = sloccount(path, datadir=datadir)
    count = 0
    for lg in ll:
        count += r['tbl'].get(lg, 0)

    return count


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='SLOCCount Driver',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('path', type=str, help='directory or file to be read')

    parser.add_argument('-l', '--lang', dest='lang', type=str, default=None,
                        help='programming language to be handled')

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
    logging.basicConfig(format='[%(levelname)s][%(funcName)s] %(message)s', level=log_level)

    if args.lang:
        c = sloccount_for_lang(args.lang, args.path)
        print(f'{c}')

    else:
        sloc = sloccount(args.path)

        for x in sloc['tbl'].items():
            print('{}: {}'.format(*x))

        print('total: {total}'.format(**sloc))


if __name__ == '__main__':
    main()
