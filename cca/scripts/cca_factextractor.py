#!/usr/bin/env python3


'''
  cca_factextractor.py

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
import sys
import logging

import pathsetup
import tp
import project
import cca_options
import factextractor
from factextractor import Enc, HashAlgo
import factutils.fileid

from factloader import make_factbase_dir

logger = logging.getLogger()


class TaskPoolBase(tp.base, factextractor.base):

    def __init__(self, proj_id, basedir='.', working_dir='.', clear_cache=True,
                 factbase_dir=None, 
                 encoding=Enc.FDLCO, algo=HashAlgo.MD5,
                 fact_out_dir=None):

        factextractor.base.__init__(self, proj_id, encoding=encoding, algo=algo)

        self._conf = project.get_conf(self._proj_id)

        if factbase_dir:
            self._factbase_dir = factbase_dir
        else:
           self._factbase_dir = make_factbase_dir(basedir, self._proj_id)
           logger.warning('factbase_dir set to "{}"'.format(self._factbase_dir))

        logger.info('factbase_dir="{}"'.format(self._factbase_dir))


        tp.base.__init__(self, working_dir, clear_cache)

        if fact_out_dir:
            self._fact_out_dir = fact_out_dir
        else:
            self._fact_out_dir = os.path.join(self.working_dir, 'fact')

        if not os.path.exists(self._fact_out_dir):
            os.makedirs(self._fact_out_dir)

        logger.info('fact_out_dir set to "{}"'.format(self._fact_out_dir))



class TaskPool(TaskPoolBase):

    def gen_tasks(self):
        tasks = []
        for i, v in enumerate(self._conf.vers):
            n = self._conf.versions[i] #self._conf.get_long_name(v)
            tasks.append((n, v))
        return tasks


class TaskPool2(TaskPoolBase):

    def gen_tasks(self):
        tasks = []

        vers = self._conf.vers
        versions = self._conf.versions

        if self._conf.vpairs:
            for (v1, v2) in self._conf.vpairs:
                n1 = self._conf.get_long_name(v1)
                n2 = self._conf.get_long_name(v2)
                tasks.append((n1, v1, n2, v2))
        else:
            for i in range(self._conf.nversions - 1):
                v0 = vers[i]
                v1 = vers[i+1]
                n0 = versions[i] #self._conf.get_long_name(v0)
                n1 = versions[i+1] #self._conf.get_long_name(v1)
                tasks.append((n0, v0, n1, v1))

        return tasks




class OptionParser(cca_options.Parser):
    def __init__(self):
        cca_options.Parser.__init__(self)

        self.argparser.add_argument('--encoding', dest='enc', default=Enc.FDLCO,
                                    help='set entity encoding to ENC (default=%default)', metavar='ENC')

        self.argparser.add_argument('--hash-algo', dest='algo', default=HashAlgo.MD5,
                                    help='set hash algorithm to ALGO (default=%default)', metavar='ALGO')

        self.argparser.add_argument('--into-directory', type=str, dest='fact_into_directory',
                                    default=None, metavar='DIR',
                                    help='dump triples into DIR (default=%default)')

