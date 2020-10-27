#!/usr/bin/env python3


'''
  A script for simple project management

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

import pathsetup
import dp


def get_confs():
    _confs = filter(lambda x: x.endswith('.py'), os.listdir(pathsetup.CONFIGS_DIR))
    confs = [os.path.splitext(x)[0] for x in _confs]
    confs.sort()
    return confs


def get_conf(name):
    m = None
    try:
        m = __import__(name)
        return m.conf
    except Exception as e:
        dp.warning('cannot find conf for "%s": %s' % (name, str(e)))
        try:
            m = __import__('cca_'+name)
            return m.conf
        except Exception as e:
            dp.warning('cannot find conf for "%s": %s' % (name, str(e)))
            return None
    

if __name__ == '__main__':
    import sys

    confs = get_confs()

    if len(sys.argv) < 2:

        confs = get_confs()

        print('%d projects are registered:' % len(confs))

        max = 0
        conf_tbl = {}
        for mname in confs:
            conf = get_conf(mname)
            if conf:
                conf_tbl[mname] = conf
                n = len(conf.proj_id)
                if n > max:
                    max = n

        fmt = '  %%-%ds (%%d versions)' % max

        for mname in confs:
            conf = conf_tbl[mname]
            print(fmt % (conf.proj_id, conf.nversions))

    else:
        proj_ids = sys.argv[1:]
        for proj_id in proj_ids:
            try:
                conf = get_conf(proj_id)
                print(conf)

            except KeyError:
                print('no such project: "%s"' % proj_id)
