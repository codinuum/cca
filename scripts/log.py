#!/usr/bin/env python3


'''
  A logging wrapper

  Copyright 2012-2017 Codinuum Software Lab <http://codinuum.com>

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
import logging



class Logger(object):
    def __init__(self, logger_name, log_dir=None):

        self.logger = logging.getLogger(logger_name)

        if len(self.logger.handlers) == 0:

            if log_dir:
                log_file = os.path.join(log_dir, logger_name+'.log')
                h = logging.FileHandler(log_file, mode='a', delay=True)
            else:
                h = logging.StreamHandler()

            h.setLevel(logging.DEBUG)
            formatter = logging.Formatter('[%(asctime)s][%(filename)s][%(funcName)s][%(levelname)s] %(message)s')
            h.setFormatter(formatter)

            self.logger.addHandler(h)

        self.logger.setLevel(logging.INFO)

        self.message = self.logger.info
        self.warning = self.logger.warning
        self.error   = self.logger.error
        self.debug   = self.logger.debug


    def set_debug_flag(self):
        self.logger.setLevel(logging.DEBUG)


def test():

    logger = Logger('test_logger')

    logger.set_debug_flag()

    logger.message('message')
    logger.warning('warning')
    logger.debug('debug')
    logger.error('error')


if __name__ == '__main__':
    test()
