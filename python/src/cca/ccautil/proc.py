#!/usr/bin/env python3

'''
  A subprocess wrapper

  Copyright 2012-2023 Codinuum Software Lab <https://codinuum.com>

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

from subprocess import Popen, PIPE, CalledProcessError
import subprocess
import logging

# from .common import setup_logger

logger = logging.getLogger()


def system(cmd, cwd=None, quiet=True, rc_check=True):
    try:
        out = open('/dev/null', 'w') if quiet else None

        p = subprocess.run(cmd, stdout=out, stderr=out, shell=True,
                           close_fds=True, cwd=cwd)
        rc = p.returncode

        if out:
            out.close()

        if rc == 0:
            return 0
        else:
            if rc_check:
                logger.warning(f'"{cmd}":'
                               f' terminated abnormally (exitcode={rc})')
            return 1

    except OSError as e:
        logger.error(f'execution failed: {e}')


def check_output(cmd, cwd=None, rc_check=True,
                 encoding=None, errors=None,
                 text=None, universal_newlines=None):
    out = None
    try:
        p = subprocess.run(cmd, shell=True, cwd=cwd, capture_output=True,
                           encoding=encoding, errors=errors,
                           text=text, universal_newlines=universal_newlines)
        out = p.stdout

    except CalledProcessError as e:
        if rc_check:
            logger.warning(f'"{cmd}":'
                           f' terminated abnormally (exitcode={e.returncode})')
        out = e.output

    return out


class PopenContext(object):
    def __init__(self, cmd, rc_check=True, stdout=PIPE, stderr=PIPE,
                 text=True, encoding='utf-8', errors='replace'):
        self.cmd = cmd
        self.rc_check = rc_check
        self.stdout = stdout
        self.stderr = stderr
        self.text = text
        self.encoding = encoding
        self.errors = errors

    def __enter__(self):
        self._po = Popen(self.cmd,
                         shell=True,
                         stdout=self.stdout,
                         stderr=self.stderr,
                         close_fds=True,
                         text=self.text,
                         encoding=self.encoding,
                         errors=self.errors)
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
                    logger.warning(f'"{self.cmd}":'
                                   f' terminated abnormally (exitcode={rc})')

            return True

        else:
            return False


def test():
    cmd = 'lss'
    c = PopenContext(cmd)
    with c as p:
        (o, e) = p.communicate()
        for li in o.split('\n'):
            print(li)


if __name__ == '__main__':
    # test()
    system('lss')
