#!/usr/bin/env python3


'''
  AST.py

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
import subprocess
import re
import logging

from .cca_config import ast_ext, compress_cmd

logger = logging.getLogger()

ast_working_dir_base = 'work.AST'

generated_srcs_info_dir_name = 'generated_sources'

bison_pat = re.compile(r'#define\s+YYBISON\s+1')
nlines_for_bison_pat_search = 32


def is_bison_generated(fpath):
    b = False
    try:
        f = open(fpath)
        count = 0
        for line in f:
            if count > nlines_for_bison_pat_search:
                break
            m = bison_pat.search(line)
            if m:
                b = True
                break
            count += 1
        f.close()
    except IOError:
        pass
    return b


flex_pat = re.compile(r'#define\s+FLEX_SCANNER')
nlines_for_flex_pat_search = 32


def is_flex_generated(fpath):
    b = False
    try:
        f = open(fpath)
        count = 0
        for line in f:
            if count > nlines_for_flex_pat_search:
                break
            m = flex_pat.search(line)
            if m:
                b = True
                break
            count += 1
        f.close()
    except IOError:
        pass
    return b


def do_cmds(dir_path, cmds):
    cwd = os.getcwd()

    os.chdir(dir_path)

    success = True

    for cmd in cmds:
        try:
            logger.info('cwd="{}"'.format(os.getcwd()))
            logger.info('executing "{}"...'.format(cmd))
            stat = subprocess.call(cmd, shell=True, close_fds=True)
            if stat != 0:
                success = False

        except KeyboardInterrupt:
            logger.warning('interrupted!')
            success = False
            break

        except BaseException as e:
            logger.warning(str(e))
            success = False
            continue

    os.chdir(cwd)

    return success


def compress_file(path):
    success = True
    try:
        if path.endswith(ast_ext):
            cmd = ' '.join((compress_cmd, path))
            subprocess.call(cmd, shell=True, close_fds=True)

    except OSError:
        logger.info('failed to compress "{}"'.format(path))
        success = False

    return success


def compress_asts(dir_path):  # bzip2 ASTs
    success = True
    try:
        names = os.listdir(dir_path)
        for name in names:
            target = os.path.join(dir_path, name)
            if os.path.isdir(target):
                success = compress_asts(target) and success
            else:
                success = compress_file(target) and success

    except OSError:
        logger.info('failed to compress ASTs in "{}"'.format(dir_path))
        success = False

    return success


# def build(conf):
#     for n in conf.versions:
#         rev_path = conf.get_ver_dir(n)
#         logger.info('building ASTs for "{}"...'.format(rev_path))
#         do_cmds(rev_path, conf.build_cmds)
#         compress_asts(rev_path)

def mkgensrcsinfofile(basedir, proj_id, n):
    p = os.path.join(basedir, ast_working_dir_base, proj_id,
                     generated_srcs_info_dir_name, n)
    return p
