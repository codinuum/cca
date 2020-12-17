#!/usr/bin/env python3


'''
  A subprocess wrapper

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

import os
from subprocess import Popen, PIPE, CalledProcessError
import subprocess
import logging

import pathsetup
from common import setup_logger

logger = logging.getLogger()


def system(cmd, cwd=None, quiet=False, rc_check=True):
    try:
        out = open('/dev/null', 'w') if quiet else None

        p = subprocess.run(cmd, stdout=out, stderr=out, shell=True, close_fds=True, cwd=cwd)
        rc = p.returncode

        if out:
            out.close()

        if rc == 0:
            return 0
        else:
            if rc_check:
                logger.warning('"%s": terminated abnormally (exitcode=%d)' % (cmd, rc))
            return 1

    except OSError as e:
        logger.error('execution failed: %s' % e)

def check_output(cmd, cwd=None, rc_check=True):
    out = None
    try:
        p = subprocess.run(cmd, shell=True, cwd=cwd, capture_output=True, universal_newlines=True)
        out = p.stdout

    except CalledProcessError as e:
        if rc_check:
            logger.warning('"%s": terminated abnormally (exitcode=%d)' % (cmd, e.returncode))
        out = e.output

    return out

class PopenContext(object):
    def __init__(self, cmd, rc_check=True, stdout=PIPE, stderr=PIPE):
        self.cmd = cmd
        self.rc_check = rc_check
        self.stdout = stdout
        self.stderr = stderr

    def __enter__(self):
        self._po = Popen(self.cmd,
                         shell=True,
                         stdout=self.stdout,
                         stderr=self.stderr,
                         close_fds=True,
                         text=True)
        return self._po

    def __exit__(self, *exc_info):
        (exc, v, tr) = exc_info

        if exc == OSError:
            logger.error('execution failed: %s' % v)
            return True

        elif exc == None:
            rc = self._po.returncode
            if rc and self.rc_check:
                if rc != 0:
                    logger.warning('"%s": terminated abnormally (exitcode=%d)' % (self.cmd, rc))
            return True

        else:
            return False
        

def test():
    cmd = 'lss'
    c = PopenContext(cmd)
    with c as p:
        (o, e) = p.communicate()
        for l in o.split('\n'):
            print(l)

if __name__ == '__main__':
    #test()
    system('lss')
