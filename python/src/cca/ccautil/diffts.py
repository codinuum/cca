#!/usr/bin/env python3

'''
  A Diff/TS Driver

  Copyright 2012-2022 Codinuum Software Lab <https://codinuum.com>

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
import re
import gzip
# import json
import rapidjson as json
import time
import logging

from . import proc
from .factextractor import Enc, HashAlgo, compute_hash
from .common import setup_logger

#####

logger = logging.getLogger()

diffts_working_dir_base = 'work.diffts'

mapfact_file_name = 'map.nt.gz'
# fact_file_name = 'fact.nt.gz'
fact_file_name_pat = re.compile(r'^fact\.nt.*')
changefact_file_name = 'changes.nt.gz'
cfgfact_file_name = 'cfg.nt.gz'


GMAP_JSON_FILE_NAME = 'gmap.json.gz'
GDIFF_JSON_FILE_NAME = 'gdiff.json.gz'


RETRY_COUNT = 3

DEFAULT_FACT_SIZE_THRESH = 10000


def gen_options():
    return {
        'preprune': True,
        'prematch': True,
        'usecache': True,
        'cache_dir_base': None,
        'load_fact': False,
        'fact_dir': None,
        'fact_versions': [],
        'fact_proj': '',
        'fact_proj_roots': [],
        'restrict_fact': False,
        'fact_for_changes': False,
        'fact_for_mapping': False,
        'fact_for_ast': False,
        'fact_into_virtuoso':  '',
        'fact_into_directory': '',
        'fact_size_thresh': DEFAULT_FACT_SIZE_THRESH,
        'dumpccs': False,
        'dironly': False,
        'check': False,
        'keep_filtered_temp': False,
        'local_cache_name': '',
        'dump_delta': False,
        'fact_for_delta': False,
        'keep_going': False,
        'quiet': False,
    }


default_options = gen_options()

diffast_preprune = True
diffast_prematch = True
diffast_usecache = True

diffast_cmd = 'diffast.opt'
patchast_cmd = 'patchast.opt'


diffts_cost_pat = re.compile(r'total changes\s*: ([0-9]+)')
diffts_nmap_pat = re.compile(r'mapping size\s*: ([0-9]+)')

diffts_insert_pat = re.compile(r'inserts[^:]*: ([0-9]+)\([0-9]+\)')
diffts_delete_pat = re.compile(r'deletes[^:]*: ([0-9]+)\([0-9]+\)')
diffts_relabel_pat = re.compile(r'relabels\s*: ([0-9]+)\(orig:[0-9]+\)\([0-9]+\)')
diffts_move_pat = re.compile(r'moves[^:]*: ([0-9]+)\([0-9]+\)')
diffts_movrel_pat = re.compile(r'mov\+rels\s*: ([0-9]+)\(orig:[0-9]+\)')

diffts_nnodes1_pat = re.compile(r'nnodes1\s*: ([0-9]+)')
diffts_nnodes2_pat = re.compile(r'nnodes2\s*: ([0-9]+)')

diffts_nnodes_pat = re.compile(r'nodes\s*: ([0-9]+)')
diffts_nastnodes_pat = re.compile(r'AST nodes\s*: ([0-9]+)')
diffts_nsrcfiles_pat = re.compile(r'source files\s*: ([0-9]+)')

diffts_LOC_pat = re.compile(r'LOC\s*: ([0-9]+)')
diffts_missed_LOC_pat = re.compile(r'missed LOC\s*: ([0-9]+)')

stat_file_name = 'stat'
info_file_name = 'info'


default_cache_dir_base = os.path.join(os.environ['HOME'], '.cca', 'cache')


def mksearchresult(cache_path, name):
    return {'cache_path': cache_path, 'path': os.path.join(cache_path, name)}


def search_cache(cache_path, name, local_cache_name=""):
    if not os.path.exists(cache_path):
        return []

    fpath = os.path.join(cache_path, name)
    paths = []
    if os.path.exists(fpath):
        paths = [{'cache_path': cache_path, 'path': fpath}]
    elif local_cache_name == "":
        pass
    else:
        bname = os.path.basename(cache_path)
        if bname == local_cache_name:
            dpath = os.path.dirname(cache_path)
            other_cache_names = os.listdir(dpath)
            for n in other_cache_names:
                cp = os.path.join(dpath, n)
                p = os.path.join(cp, name)
                if os.path.exists(p):
                    paths.append({'cache_path': cp, 'path': p})

    return paths


def set_value(result, key, pat, line):
    m = pat.search(line)
    if m:
        try:
            result[key] = int(m.group(1))
        except Exception:
            logger.warning(f'cannot get value: key="{key}" line="{line}"')


def do_cmd(cmd):
    for i in range(RETRY_COUNT):
        stat = proc.system(cmd)
        if stat == 0:
            break

        time.sleep(1)
        logger.info(f'retrying...({i}) cmd="{cmd}"')


#####

def get_cache_dir1(a,
                   cache_dir_base=None,
                   local_cache_name=None,
                   algo=HashAlgo.MD5
                   ):
    h = compute_hash(algo, a)
    d = h[0:2]
    if cache_dir_base:
        cache_dir = os.path.join(cache_dir_base, d, h)
    else:
        cache_dir = os.path.join(default_cache_dir_base, d, h)

    if local_cache_name:
        cache_dir = os.path.join(cache_dir, local_cache_name)

    return cache_dir


def _get_cache_dir(a1, a2,
                   cache_dir_base=None,
                   local_cache_name=None,
                   algo=HashAlgo.MD5
                   ):
    h1 = compute_hash(algo, a1)
    h2 = compute_hash(algo, a2)
    d = h1[0:2]

    c = f'{h1}-{h2}'

    if cache_dir_base:
        cache_dir = os.path.join(cache_dir_base, d, c)
    else:
        cache_dir = os.path.join(default_cache_dir_base, d, c)

    if local_cache_name:
        cache_dir = os.path.join(cache_dir, local_cache_name)

    return cache_dir


def get_cache_dir1_(diff_cmd, a,
                    cache_dir_base=None,
                    local_cache_name=None,
                    quiet=False,
                    algo=HashAlgo.MD5
                    ):

    cache_opt = ''
    if cache_dir_base:
        cache_opt = f' -cache {cache_dir_base}'
    if local_cache_name:
        cache_opt += f' -localcachename {local_cache_name}'

    hash_opt = ' -fact:hash:' + algo

    opts = cache_opt + hash_opt

    cmd = f'{diff_cmd} -parseonly{opts} -getcache "{a}"'

    if not quiet:
        logger.info(f'cmd: "{cmd}"')

    cache_dir = None

    pc = proc.PopenContext(cmd)
    with pc as p:
        (o, e) = p.communicate()
        cache_dir = o.strip()

    return cache_dir


def get_cache_dir(diff_cmd, a1, a2,
                  cache_dir_base=None,
                  local_cache_name=None,
                  quiet=False,
                  algo=HashAlgo.MD5
                  ):

    cache_opt = ''
    if cache_dir_base:
        cache_opt = f' -cache {cache_dir_base}'
    if local_cache_name:
        cache_opt += f' -localcachename {local_cache_name}'

    hash_opt = ' -fact:hash:' + algo

    opts = cache_opt + hash_opt

    cmd = f'{diff_cmd}{opts} -getcache "{a1}" "{a2}"'

    if not quiet:
        logger.info(f'cmd: "{cmd}"')

    cache_dir = None

    pc = proc.PopenContext(cmd)
    with pc as p:
        (o, e) = p.communicate()
        cache_dir = o.strip()

    return cache_dir


def get_fact_versions_opt(fact_versions):
    li = []
    for v in fact_versions:
        li.append(f'-fact:version {v}')

    return ' '.join(li)


def get_fact_proj_roots_opt(fact_proj_roots):
    li = []
    for r in fact_proj_roots:
        li.append(f'-fact:project-root {r}')

    return ' '.join(li)


def read_json(data_path):
    d = None
    _open = open
    if data_path.endswith('.gz'):
        _open = gzip.open
    try:
        with _open(data_path, 'r') as f:
            d = json.load(f)
    except Exception as e:
        logger.warning(f'{data_path}: {e}')

    return d


def read_file(r, name_pat_list, stat_paths, retry_count=RETRY_COUNT):
    count = 0
    stat_paths = stat_paths * (int(retry_count / len(stat_paths)) + 1)
    for stat in stat_paths:
        try:
            f = open(stat['path'])
            for ln in f:
                for (name, pat) in name_pat_list:
                    set_value(r, name, pat, ln)
            f.close()
            break

        except IOError as e:
            logger.warning(str(e))
            logger.info(f'retrying...({count})')
            time.sleep(1)
            count += 1
            continue


def read_file_info_file(stat_paths, retry_count=RETRY_COUNT):
    r = {'nnodes': 0,
         'loc': 0,
         'missed_loc': 0,
         }
    li = [('nnodes',     diffts_nnodes_pat),
          ('loc',        diffts_LOC_pat),
          ('missed_loc', diffts_missed_LOC_pat),
          ]

    read_file(r, li, stat_paths, retry_count)

    return r


def read_dir_info_file(stat_paths, retry_count=RETRY_COUNT):
    r = {'nnodes': 0,
         'nastnodes': 0,
         'nsrcfiles': 0,
         }
    li = [('nnodes',    diffts_nnodes_pat),
          ('nastnodes', diffts_nastnodes_pat),
          ('nsrcfiles', diffts_nsrcfiles_pat),
          ]

    read_file(r, li, stat_paths, retry_count)

    return r


def read_file_diff_stat_file(stat_paths, retry_count=RETRY_COUNT):
    r = {
        'cost': 0,
        'nmappings': 0,
        'ninserts': 0,
        'ndeletes': 0,
        'nrelabels': 0,
        'nmoves': 0,
        'nmovrels': 0,
        'nnodes1': 0,
        'nnodes2': 0,
    }
    li = [('cost',      diffts_cost_pat),
          ('nmappings', diffts_nmap_pat),
          ('ninserts',  diffts_insert_pat),
          ('ndeletes',  diffts_delete_pat),
          ('nrelabels', diffts_relabel_pat),
          ('nmoves',    diffts_move_pat),
          ('nmovrels',  diffts_movrel_pat),
          ('nnodes1',   diffts_nnodes1_pat),
          ('nnodes2',   diffts_nnodes2_pat),
          ]

    read_file(r, li, stat_paths, retry_count)

    return r


def write_stat_file(stat, path):
    fmt = '''nnodes1: %(nnodes1)d
nnodes2: %(nnodes2)d
deletes  : %(ndeletes)d
inserts  : %(ninserts)d
relabels : %(nrelabels)d
total changes : %(cost)d
mapping size  : %(nmapping)d
'''
    s = fmt % stat
    try:
        f = open(path, 'w')
        f.write(s)
    except Exception as e:
        logger.warning(str(e))
    finally:
        if f:
            f.close


def diffts(diff_cmd, file1, file2,
           preprune=True,
           prematch=True,
           usecache=True,
           cache_dir_base=None,
           load_fact=False,
           fact_dir=None,
           fact_versions=[],
           fact_proj='',
           fact_proj_roots=[],
           restrict_fact=False,
           fact_for_changes=False,
           fact_for_mapping=False,
           fact_for_ast=False,
           fact_into_virtuoso='',
           fact_into_directory='',
           fact_size_thresh=DEFAULT_FACT_SIZE_THRESH,
           fact_encoding=Enc.FDLCO,
           fact_hash_algo=HashAlgo.MD5,
           fact_no_compress=False,
           dumpccs=False,
           dironly=False,
           check=False,
           aggressive=False,
           ignore_moves_of_unordered=False,
           no_unnamed_node_moves=False,
           keep_filtered_temp=False,
           local_cache_name='',
           dump_delta=False,
           fact_for_delta=False,
           keep_going=False,
           quiet=False):

    logger.info(f'comparing "{file1}" with "{file2}"')

    cache_dir = get_cache_dir(diff_cmd, file1, file2, cache_dir_base,
                              local_cache_name, quiet=quiet, algo=fact_hash_algo)

    stat_paths = search_cache(cache_dir, stat_file_name, local_cache_name)

    if load_fact or stat_paths == [] or not usecache:

        logger.info(f'diff_cmd: {diff_cmd}')

        prep_opt = ''
        if preprune:
            logger.info('prepruning enabled')
        else:
            logger.info('prepruning disabled')
            prep_opt = ' -nopreprune'

        prem_opt = ''
        if prematch:
            logger.info('prematching enabled')
        else:
            logger.info('prematching disabled')
            prem_opt = ' -noprematch'

        cache_opt = ' -clearcache'
        if usecache:
            logger.info('using cache')
            cache_opt = ' -usecache'

        fact_opt = ''

        if load_fact:
            logger.info('loading fact')
            if fact_versions:
                fact_opt = f' -fact -fact:add-versions {get_fact_versions_opt(fact_versions)}'

                fact_opt += ' -fact:encoding:' + fact_encoding
                fact_opt += ' -fact:hash:' + fact_hash_algo

                if restrict_fact:
                    fact_opt += ' -fact:restricted'
                if fact_for_changes:
                    fact_opt += ' -fact:changes:basic'
                if fact_for_mapping:
                    fact_opt += ' -fact:mapping'
                if fact_for_ast:
                    fact_opt += ' -fact:ast'

                if fact_no_compress:
                    fact_opt += ' -fact:nocompress'

                if fact_proj:
                    fact_opt += f' -fact:project {fact_proj}'

                if fact_proj_roots:
                    fact_opt += f' {get_fact_proj_roots_opt(fact_proj_roots)}'

                if fact_into_virtuoso:
                    fact_opt += f' -fact:into-virtuoso {fact_into_virtuoso}'

                if fact_into_directory:
                    fact_opt += f' -fact:into-directory {fact_into_directory}'

                if fact_for_delta:
                    fact_opt += ' -fact:delta'

                fact_opt += f' -fact:size-thresh {fact_size_thresh}'

            else:
                logger.error('specify fact_versions')

#        if not os.path.exists(diffts_working_dir_base):
#            os.makedirs(diffts_working_dir_base)

        cachedir_opt = ''
        if cache_dir_base:
            logger.info(f'cache dir base: "{cache_dir_base}"')
            cachedir_opt = f' -cache {cache_dir_base}'

        dumpccs_opt = ''
        if dumpccs:
            logger.info('dumping CCS')
            dumpccs_opt = ' -dump:ccs'

        check_opt = ''
        if check:
            logger.info('checking result')
            check_opt = ' -check'

        other_opts = ''
        if aggressive:
            other_opts += ' -aggressive'

        if no_unnamed_node_moves:
            other_opts += ' -no-unnamed-node-moves'

        if ignore_moves_of_unordered:
            other_opts += ' -ignore-moves-of-unordered'

        if keep_filtered_temp:
            logger.info('keep filtered temp files')
            other_opts += ' -keep-filtered-temp-file'

        if keep_going:
            other_opts += ' -k'

        if local_cache_name:
            logger.info(f'local cache name: "{local_cache_name}"')
            other_opts += f' -localcachename {local_cache_name}'

        if dump_delta:
            other_opts += ' -dump:delta'

        cmd = ''.join((diff_cmd,
                       cache_opt, cachedir_opt, prep_opt, prem_opt, fact_opt,
                       dumpccs_opt, check_opt, other_opts))
        cmd += f' "{file1}" "{file2}"'

        logger.info(f'cmd="{cmd}"')

        proc.system(cmd, quiet=quiet)

    r = {
        'cost': 0,
        'nmappings': 0,
        'ninserts': 0,
        'ndeletes': 0,
        'nrelabels': 0,
        'nmoves': 0,
        'nmovrels': 0,
        'nnodes1': 0,
        'nnodes2': 0,
        # 'exitcode': 0,
        }

    if dironly:
        return r

    stat_paths = [mksearchresult(cache_dir, stat_file_name)]

    r = read_file_diff_stat_file(stat_paths, RETRY_COUNT)

    return r


def diffast(file1, file2, **options):
    return diffts(diffast_cmd, file1, file2, **options)


def diffast_get_cache_dir1(file, **options):
    opts = options.copy()
    if 'usecache' in opts:
        del opts['usecache']
    if 'quiet' not in opts:
        opts['quiet'] = True
    return get_cache_dir1_(diffast_cmd, file, **opts)


def diffast_get_cache_dir(file1, file2, **options):
    opts = options.copy()
    if 'usecache' in opts:
        del opts['usecache']
    if 'quiet' not in opts:
        opts['quiet'] = True
    return get_cache_dir(diffast_cmd, file1, file2, **opts)


def diffast_get_gmap(file1, file2, **options):
    cache_dir = diffast_get_cache_dir(file1, file2, **options)
    gmap_json = os.path.join(cache_dir, GMAP_JSON_FILE_NAME)
    d = read_json(gmap_json)
    return d


def diffast_get_gdiff(file1, file2, **options):
    cache_dir = diffast_get_cache_dir(file1, file2, **options)
    gdiff_json = os.path.join(cache_dir, GDIFF_JSON_FILE_NAME)
    d = read_json(gdiff_json)
    return d


def dump_unparsed(path, to_path, quiet=False):
    cmd = f'{diffast_cmd} -clearcache -parseonly -dump:src:out "{to_path}" "{path}"'
    if not quiet:
        logger.info('cmd="{}"'.format(cmd))

    return proc.system(cmd, quiet=quiet)


def patchast(path, delta_path, out_path, quiet=False):
    cmd = f'{patchast_cmd} -o "{out_path}" "{path}" "{delta_path}"'
    if not quiet:
        logger.info(f'cmd="{cmd}"')

    return proc.system(cmd, quiet=quiet)


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    argparser = ArgumentParser(description='A Diff/TS driver',
                               formatter_class=ArgumentDefaultsHelpFormatter)

    argparser.add_argument('file1', type=str)
    argparser.add_argument('file2', type=str)

    argparser.add_argument('-d', '--debug', action='store_true', dest='debug',
                           default=False, help='enable debug output')

    argparser.add_argument('--nopreprune', action='store_false',
                           dest='preprune', default=True,
                           help='disable prepruning')

    argparser.add_argument('--noprematch', action='store_false',
                           dest='prematch', default=True,
                           help='disable prematching')

    argparser.add_argument('-c', '--cachebase', dest='cachebase',
                           metavar='PATH', default=None,
                           help='set cache base to PATH')

    args = argparser.parse_args()

    log_level = logging.WARNING
    if args.debug:
        log_level = logging.DEBUG

    setup_logger(logger, log_level)

    mode = 'ast'
    f1 = args.file1
    f2 = args.file2

    r = None

    logger.info(f'mode: "{mode}"')

    if mode == 'ast':
        r = diffast(f1, f2, preprune=args.preprune, prematch=args.prematch,
                    cache_dir_base=args.cachebase)
    else:
        logger.error('invalid mode')

    if r:
        cost = r['cost']
        nmappings = r['nmappings']
        cmr = float(cost) / float(nmappings)

        print(f'cost: {cost} nmappings: {nmappings} CMR:{cmr}')

    else:
        logger.error('failed')


if __name__ == '__main__':
    main()
