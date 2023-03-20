#!/usr/bin/env python3


'''
  srcdiff.py

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

import sys
import os
import re
import time
import rapidjson as json
import bz2
import csv
import xml.parsers.expat as expat
import random
import logging

from . import diffts, sim, proc
from .factextractor import Enc, HashAlgo, compute_hash
from .common import setup_logger

#####

logger = logging.getLogger()

AUXFILE_EXTS = ['.jj', '.jjt', '.properties']

dirdiff_fact_file_name = 'fact.nt'

ccs_ext = '.ccs'

astml_exts = ('.ast', '.ast.bz2')

comp_exts = ('.bz2')

INFO_JSON = 'info.json'

#####


def is_auxfile(fname):
    for ext in AUXFILE_EXTS:
        if fname.endswith(ext):
            return True
    return False

# file_hash = diffts.file_hash


def file_hash(path):
    return compute_hash(HashAlgo.MD5, path)


nodes_pat = re.compile('nodes: ([0-9]+)')


diffast = diffts.diffast


def diffast_nopreprune(file1, file2):
    return diffts.diffast(file1, file2, preprune=False, quiet=True)


class DiffTSError(Exception):
    pass


def get_from_cache(cache_dir, xxx, local_cache_name):
    fs = diffts.search_cache(cache_dir, xxx, local_cache_name)
    # logger.info(f'{cache_dir} for {xxx} (local_cache_name={local_cache_name}) --> {fs}')
    return fs


def read_info(info_paths):
    i = diffts.read_file_info_file(info_paths)
    return i['nnodes']


def count_nodes(files, cache_dir_base=None, usecache=True,
                load_fact=False,
                fact_dir=None,
                fact_versions=[],
                fact_proj='',
                fact_proj_roots=[],
                fact_for_ast=False,
                fact_into_virtuoso='',
                fact_into_directory='',
                fact_size_thresh=diffts.DEFAULT_FACT_SIZE_THRESH,
                restrict_fact=False,
                fact_encoding=Enc.FDLCO,
                fact_hash_algo=HashAlgo.MD5,
                local_cache_name=None):

    nfiles = len(files)
    file_count = 0
    c = 0
    for f in files:

        if is_auxfile(f):
            logger.debug(f'pre-source "{f}" is ignored')
            continue

        file_count += 1
        logger.debug(f'*** counting nodes in files ({file_count}/{nfiles})')

        logger.debug(f'"{f}"')

        logger.debug(f'cache_dir_base: "{cache_dir_base}"')

        cache_path = diffts.get_cache_dir1(f, cache_dir_base, local_cache_name)
        info_paths = get_from_cache(cache_path, diffts.info_file_name,
                                    local_cache_name)

        if usecache and not load_fact and info_paths:
            n = read_info(info_paths)
            logger.debug(f'number of nodes: {f} --> {n} (cached)')
            c += n
            continue

        cache_opt = ''
        if cache_dir_base:
            cache_opt = f' -cache {cache_dir_base}'
        if local_cache_name:
            cache_opt += f' -localcachename {local_cache_name}'

        fact_opt = ''
        if load_fact:
            logger.debug('loading fact')
            if fact_versions:
                fact_opt = ' -fact -fact:add-versions'

                fact_opt += f' {diffts.get_fact_versions_opt(fact_versions)}'

                fact_opt += ' -fact:encoding:' + fact_encoding
                fact_opt += ' -fact:hash:' + fact_hash_algo

                if restrict_fact:
                    fact_opt += ' -fact:restricted'

                if fact_for_ast:
                    fact_opt += ' -fact:ast'

                if fact_proj:
                    fact_opt += ' -fact:project ' + fact_proj

                if fact_proj_roots:
                    fact_opt += ' ' + diffts.get_fact_proj_roots_opt(fact_proj_roots)

                if fact_into_virtuoso:
                    fact_opt += f' -fact:into-virtuoso {fact_into_virtuoso}'

                if fact_into_directory:
                    fact_opt += f' -fact:into-directory {fact_into_directory}'

                fact_opt += f' -fact:size-thresh {fact_size_thresh}'
            else:
                logger.error('specify fact_versions')

        incomplete_opt = ''
        if not load_fact:
            incomplete_opt = ' -incompleteinfo'

        cmd = f'{diffts.diffast_cmd}{incomplete_opt} -parseonly{cache_opt}{fact_opt} "{f}"'

        logger.debug(f'cmd="{cmd}"')

        pc = proc.PopenContext(cmd)
        with pc as p:
            (stdout_data, stderr_data) = p.communicate()
            for line in stdout_data.split('\n'):
                m = nodes_pat.search(line)
                if m:
                    g = m.groups()
                    try:
                        n = int(g[0])
                        logger.debug(f'number of nodes: {f} --> {n}')
                        c += n
                        break
                    except Exception:
                        logger.warning(f'not an integer: "{g[0]}"')

    return c


get_cache_dir1_by_diffts = diffts.diffast_get_cache_dir1
get_cache_dir_by_diffts = diffts.diffast_get_cache_dir


def get_cache_dir(a1, a2, cache_dir_base=None, local_cache_name=None,
                  algo=HashAlgo.MD5):
    if os.path.isdir(a1) or os.path.isdir(a2):
        return get_cache_dir_by_diffts(a1, a2,
                                       cache_dir_base=cache_dir_base,
                                       local_cache_name=local_cache_name,
                                       algo=algo)

    else:
        return diffts._get_cache_dir(a1, a2, cache_dir_base, local_cache_name,
                                     algo)


def read_stat2(fname, roots=[]):
    logger.debug(f'reading "{fname}"...')
    result = []
    try:
        f = open(fname)
        reader = csv.reader(f)

        for row in reader:
            logger.debug(f'row={row}')
            if len(roots) > 1:
                p1 = os.path.join(roots[0], row[0])
                p2 = os.path.join(roots[1], row[1])
            else:
                p1 = os.path.abspath(row[0])
                p2 = os.path.abspath(row[1])

            result.append((p1, p2))

        f.close()

        result = result[1:]  # ignore header

    except IOError as e:
        logger.warning(str(e))

    return result


def read_stat1(fname, root=None):
    result = []
    try:
        f = open(fname)
        for line in f:
            if root:
                p = os.path.join(root, line.rstrip())
            else:
                p = os.path.abspath(line.rstrip())
            result.append(p)
        f.close()

    except IOError as e:
        logger.warning(str(e))

    result = result[1:]  # ignore header

    return result


def read_stat_except_first(fname, root=None):
    logger.debug(f'reading "{fname}"...')
    result = []
    try:
        f = open(fname)
        reader = csv.reader(f)

        for row in reader:
            logger.debug(f'row={row}')

            for x in row[1:]:
                if root:
                    p = os.path.join(root, x)
                else:
                    p = os.path.abspath(x)
                result.append(p)

        f.close()

        result = result[1:]  # ignore header

    except IOError as e:
        logger.warning(str(e))

    return result


def read_stat_except_last(fname, root=None):
    logger.debug(f'reading "{fname}"...')
    result = []
    try:
        f = open(fname)
        reader = csv.reader(f)

        for row in reader:
            logger.debug(f'row={row}')

            for x in row[0:-1]:
                if root:
                    p = os.path.join(root, x)
                else:
                    p = os.path.abspath(x)
                result.append(p)

        f.close()

        result = result[1:]  # ignore header

    except IOError as e:
        logger.warning(str(e))

    return result


def get_stat_files(cache_dir, local_cache_name):
    fs = diffts.search_cache(cache_dir, diffts.stat_file_name, local_cache_name)
    return fs


# def get_map_f(cache_dir):
#     f = os.path.join(cache_dir, 'map')
#     return f


def same_file(f1, f2):
    return file_hash(f1) == file_hash(f2)


def same_ast(f1, f2, cache_dir_base=None, local_cache_name=None):  # cache based
    try:
        if same_file(f1, f2):
            return True
        else:
            cdir = get_cache_dir(f1, f2, cache_dir_base, local_cache_name)
            stat_paths = get_stat_files(cdir, local_cache_name)
            stat = diffts.read_file_diff_stat_file(stat_paths)
            return stat['cost'] == 0

    except BaseException as e:
        logger.error(str(e))


def get_info(dir1, dir2, usecache=True, cache_dir_base=None,
             load_fact=False,
             fact_dir=None,
             fact_versions=[],
             fact_proj='',
             fact_proj_roots=[],
             fact_for_changes=False,
             fact_for_mapping=False,
             fact_into_virtuoso='',
             fact_into_directory='',
             fact_size_thresh=diffts.DEFAULT_FACT_SIZE_THRESH,
             fact_for_cfg=False,
             fact_encoding=Enc.FDLCO,
             fact_hash_algo=HashAlgo.MD5,
             local_cache_name=None,
             fact_for_delta=False,
             keep_going=False,
             quiet=True):

    cache_dir = get_cache_dir(dir1, dir2, cache_dir_base, local_cache_name)

    logger.info(f'usecache: "{usecache}"')
    logger.info(f'cache_dir: "{cache_dir}"')

    logger.info('checking cache...')

    req = [('mod', 'modified.csv'),
           ('unmod', 'unmodified.csv'),
           ('renamed', 'renamed.csv'),
           ('moved', 'moved.csv'),
           ('removed', 'removed.csv'),
           ('added', 'added.csv'),
           ('copied', 'copied.csv'),
           ('glued', 'glued.csv'),
           ]

    required = {}

    cache_found = True

    for (key, name) in req:
        li = get_from_cache(cache_dir, name, local_cache_name)
        if li:
            required[key] = li[0]['path']
        else:
            logger.info(f'not found: "{name}"')
            cache_found = False
            break

    if usecache and cache_found:
        logger.info('cache found')
    else:
        diffast(dir1, dir2, usecache=usecache, cache_dir_base=cache_dir_base,
                load_fact=load_fact,
                fact_dir=fact_dir,
                fact_versions=fact_versions,
                fact_proj=fact_proj,
                fact_proj_roots=fact_proj_roots,
                fact_for_changes=fact_for_changes,
                fact_for_mapping=fact_for_mapping,
                fact_into_virtuoso=fact_into_virtuoso,
                fact_into_directory=fact_into_directory,
                fact_size_thresh=fact_size_thresh,
                fact_encoding=fact_encoding,
                fact_hash_algo=fact_hash_algo,
                dironly=True,
                local_cache_name=local_cache_name,
                fact_for_delta=fact_for_delta,
                keep_going=keep_going,
                quiet=quiet)

        for (key, name) in req:
            required[key] = os.path.join(cache_dir, name)

    if fact_proj_roots:
        roots = fact_proj_roots
        root1 = roots[0]
        root2 = roots[1]
    else:
        roots = [dir1, dir2]
        root1 = dir1
        root2 = dir2

    modified_pairs = read_stat2(required['mod'], roots=roots)
    unmodified = read_stat2(required['unmod'], roots=roots)
    renamed = read_stat2(required['renamed'], roots=roots)
    moved = read_stat2(required['moved'], roots=roots)
    removed = read_stat1(required['removed'], root=root1)
    added = read_stat1(required['added'], root=root2)
    copied = read_stat_except_first(required['copied'], root=root2)
    glued = read_stat_except_last(required['glued'], root=root1)

    result = {
        'modified': modified_pairs,
        'unmodified': unmodified,
        'added': added,
        'removed': removed,
        'renamed': renamed,
        'moved': moved,
        'copied': copied,
        'glued': glued,
        'cache_dir': cache_dir,
    }

    return result


class NotNull(Exception):
    pass


def is_compressed(f):
    b = False
    for comp_ext in comp_exts:
        if f.endswith(comp_ext):
            b = True
            break
    return b


def null_astml(astml_path):
    global null_astml_count
    null_astml_count = 0

    def start_element(name, attrs):
        global null_astml_count
        null_astml_count += 1
        if null_astml_count > 1:
            raise NotNull

    try:
        if is_compressed(astml_path):
            f = bz2.BZ2File(astml_path)
        else:
            f = open(astml_path, 'r')
        xmlparser = expat.ParserCreate()
        xmlparser.StartElementHandler = start_element
        xmlparser.ParseFile(f)
        f.close()

    except NotNull:
        return False

    except IOError as e:
        logger.warning(str(e))

    b = null_astml_count == 1

    return b


def has_AST(f):
    b = False
    for astml_ext in astml_exts:
        astml = f + astml_ext
        if os.path.exists(astml) and not null_astml(astml):
            b = True
            break
    return b


def filter_sources(sources, ignore=[], get_rel=lambda x: x, filt=lambda x: True):
    result = []
    for f in sources:

        if f in ignore:
            continue

        if not filt(get_rel(f)):
            continue

        result.append(f)

    return result


def filter_pairs(pairs, ignore1=[], ignore2=[],
                 get_rel1=lambda x: x, get_rel2=lambda x: x,
                 filt=lambda x: True):

    logger.info(f'size of pairs: {len(pairs)}')

    result = []
    for (f1, f2) in pairs:
        if f1 in ignore1 or f2 in ignore2:
            continue

        if not filt(get_rel1(f1)) or not filt(get_rel2(f2)):
            continue

        result.append((f1, f2))

    logger.info(f'size of filtered pairs: {len(result)}')

    return result


def diff_dirs(diff, dir1, dir2, usecache=True, cache_dir_base=None, use_result_cache=False,
              include=[],
              exclude=[],
              ignore1=[], ignore2=[],
              load_fact=False,
              fact_dir=None,
              fact_versions=[],
              fact_proj='',
              fact_proj_roots=[],
              ignore_unmodified=False,
              restrict_fact=False,
              fact_for_changes=False,
              fact_for_mapping=False,
              fact_for_ast=False,
              fact_into_virtuoso='',
              fact_into_directory='',
              fact_size_thresh=diffts.DEFAULT_FACT_SIZE_THRESH,
              fact_for_cfg=False,
              fact_encoding=Enc.FDLCO,
              fact_hash_algo=HashAlgo.MD5,
              fact_no_compress=False,
              line_sim=False,
              dumpccs=False,
              check=False,
              aggressive=False,
              ignore_moves_of_unordered=False,
              no_unnamed_node_moves=False,
              keep_filtered_temp=False,
              local_cache_name=None,
              dump_delta=False,
              fact_for_delta=False,
              keep_going=False,
              use_sim=False,
              sim_thresh=0.7,
              quiet=False,
              no_node_count=False,
              ):

    filt = (lambda x: True)

    if include:
        filt = (lambda x: any(x.startswith(p) for p in include))

    if exclude:
        if include:
            filt = (lambda x:
                    any(x.startswith(p) for p in include)
                    and all(not x.startswith(p) for p in exclude))
        else:
            filt = (lambda x: all(not x.startswith(p) for p in exclude))

    logger.info(f'"{dir1}" - "{dir2}" cache_dir_base="{cache_dir_base}"')

    cost = 0
    nmappings = 0
    nnodes = 0
    nnodes1 = 0
    nnodes2 = 0
    nrelabels = 0
    nmoves = 0
    nmovrels = 0

    line_sim_sum = 0.0
    line_sim_count = 0

    info = get_info(dir1, dir2, usecache=usecache,
                    cache_dir_base=cache_dir_base,
                    load_fact=load_fact,
                    fact_dir=fact_dir,
                    fact_versions=fact_versions,
                    fact_proj=fact_proj,
                    fact_proj_roots=fact_proj_roots,
                    fact_for_changes=fact_for_changes,
                    fact_for_cfg=fact_for_cfg,
                    fact_for_mapping=fact_for_mapping,
                    fact_into_virtuoso=fact_into_virtuoso,
                    fact_into_directory=fact_into_directory,
                    fact_size_thresh=fact_size_thresh,
                    fact_encoding=fact_encoding,
                    fact_hash_algo=fact_hash_algo,
                    local_cache_name=local_cache_name,
                    fact_for_delta=fact_for_delta,
                    keep_going=keep_going,
                    quiet=quiet)

    logger.info(f'"{dir1}" - "{dir2}" get_info finished')

    cache_dir_info_json = os.path.join(info['cache_dir'], INFO_JSON)
    if use_result_cache and os.path.exists(cache_dir_info_json):
        with open(cache_dir_info_json) as f:
            res = json.load(f)
        return res

    get_rel1 = (lambda x: x)
    get_rel2 = (lambda x: x)
    if len(fact_proj_roots) == 2:
        (d1, d2) = fact_proj_roots
        pat1 = re.compile(r'^{}{}'.format(d1.rstrip(os.path.sep), os.path.sep))
        pat2 = re.compile(r'^{}{}'.format(d2.rstrip(os.path.sep), os.path.sep))
        get_rel1 = (lambda x: pat1.sub('', x))
        get_rel2 = (lambda x: pat2.sub('', x))

    modified = filter_pairs(info['modified'],
                            ignore1, ignore2, get_rel1, get_rel2, filt)
    unmodified = filter_pairs(info['unmodified'],
                              ignore1, ignore2, get_rel1, get_rel2, filt)
    renamed = filter_pairs(info['renamed'],
                           ignore1, ignore2, get_rel1, get_rel2, filt)
    moved = filter_pairs(info['moved'],
                         ignore1, ignore2, get_rel1, get_rel2, filt)

    added = filter_sources(info['added'], ignore2, get_rel2, filt)
    copied = filter_sources(info['copied'], ignore2, get_rel2, filt)
    removed = filter_sources(info['removed'], ignore1, get_rel1, filt)
    glued = filter_sources(info['glued'], ignore1, get_rel1, filt)

    extra_pairs = []
    if use_sim:
        logger.debug('matching removed and added files...')
        li = []
        for x in removed:
            logger.debug(f'{x}')
            cs_ = []
            for x_ in added:
                s = sim.sim(x, x_)
                if s > sim_thresh:
                    logger.debug(f'  {x_} ({s})')
                    cs_.append((x_, s))
            if cs_:
                li.append((x, cs_))
        pairs = set()
        pairs0 = set()
        for (x, cs_) in li:
            if len(cs_) == 1:
                pairs.add((x, cs_[0][0]))
            else:
                pairs0.add((x, max(cs_, key=lambda x: x[1])[0]))

        l_ = []
        for x_ in added:
            logger.debug(f'{x_}')
            cands = []
            for x in removed:
                s = sim.sim(x, x_)
                if s > sim_thresh:
                    logger.debug(f'  {x} ({s})')
                    cands.append((x, s))
            if cands:
                l_.append((cands, x_))
        pairs_ = set()
        pairs0_ = set()
        for (cs, x_) in l_:
            if len(cs) == 1:
                pairs_.add((cs[0][0], x_))
            else:
                pairs0_.add((max(cs, key=lambda x: x[1])[0], x_))

        extra_pairs = list((pairs & pairs_) | (pairs0 & pairs0_))

        logger.info(f'extra pairs (sim_thresh={sim_thresh}):')
        for p in extra_pairs:
            logger.info('  {} - {}'.format(*p))

    if extra_pairs:
        for p in extra_pairs:
            (x, x_) = p
            removed.remove(x)
            added.remove(x_)
            modified.append(p)

    modified0 = [p[0] for p in modified]
    unmodified0 = [p[0] for p in unmodified]
    moved0 = [p[0] for p in moved]
    renamed0 = [p[0] for p in renamed]

    # for multi-processing
    random.shuffle(unmodified0)
    random.shuffle(moved0)
    random.shuffle(renamed0)
    random.shuffle(added)
    random.shuffle(copied)
    random.shuffle(removed)
    random.shuffle(glued)

    count_opts = {
        'usecache': usecache,
        'cache_dir_base': cache_dir_base,
        'load_fact': load_fact,
        'fact_dir': fact_dir,
        'fact_versions': fact_versions,
        'fact_proj': fact_proj,
        'fact_proj_roots': fact_proj_roots,
        'fact_for_ast': fact_for_ast,
        'fact_into_virtuoso': fact_into_virtuoso,
        'fact_into_directory': fact_into_directory,
        'fact_size_thresh': fact_size_thresh,
        'restrict_fact': restrict_fact,
        'fact_encoding': fact_encoding,
        'fact_hash_algo': fact_hash_algo,
        'local_cache_name': local_cache_name,
    }

    modified0set = set(modified0)
    moved0set = set(moved0)
    renamed0set = set(renamed0)
    if ignore_unmodified:
        nunmodified0 = 0
        nmoved0 = len(moved0set - modified0set)
        nrenamed0 = len(renamed0set - modified0set)
    elif no_node_count:
        nunmodified0 = 0
        nmoved0 = 0
        nrenamed0 = 0
    else:
        nunmodified0 = count_nodes(unmodified0, **count_opts)
        nmoved0 = count_nodes(moved0set - modified0set, **count_opts)
        nrenamed0 = count_nodes(renamed0set - modified0set, **count_opts)

    fvs0 = []
    fvs1 = []
    if len(fact_versions) == 2 and load_fact:
        fvs0 = [fact_versions[0]]
        fvs1 = [fact_versions[1]]

    fpr0 = []
    fpr1 = []
    if len(fact_proj_roots) == 2 and load_fact:
        fpr0 = [fact_proj_roots[0]]
        fpr1 = [fact_proj_roots[1]]

    count_opts['fact_versions'] = fvs1
    count_opts['fact_proj_roots'] = fpr1

    if no_node_count:
        nadded = 0
        ncopied = 0
    else:
        nadded = count_nodes(added, **count_opts)
        ncopied = count_nodes(copied, **count_opts)

    count_opts['fact_versions'] = fvs0
    count_opts['fact_proj_roots'] = fpr0

    if no_node_count:
        nremoved = 0
        nglued = 0
    else:
        nremoved = count_nodes(removed, **count_opts)
        nglued = count_nodes(glued, **count_opts)

    d_nnodes1 = nunmodified0 + nmoved0 + nrenamed0 + nremoved + nglued
    d_nnodes2 = nunmodified0 + nmoved0 + nrenamed0 + nadded + ncopied

    nnodes1 += d_nnodes1
    nnodes2 += d_nnodes2
    nnodes += d_nnodes1 + d_nnodes2

    nmappings += nunmodified0 + nmoved0 + nrenamed0
    cost += nadded + ncopied + nremoved + nglued

    logger.info(f'nnodes={nnodes}, nmappings={nmappings}, cost={cost}')

    st_time = time.time()

    try:
        modified_all = modified

        logger.info(f'{len(modified_all)} modified files')

        random.shuffle(modified_all)  # for multi-processing

        n_modified_all = len(modified_all)

        count = 0

        for (file1, file2) in modified_all:

            if is_auxfile(file1):
                logger.debug(f'pre-source "{file1}" is ignored')
                continue

            if is_auxfile(file2):
                logger.debug(f'pre-source "{file2}" is ignored')
                continue

            count += 1

            logger.info(f'*** processing modified files ({count}/{n_modified_all})')

            if line_sim:
                line_sim_sum += sim.line_sim(file1, file2)
                line_sim_count += 1

            st0 = time.time()

            r = diff(file1, file2,
                     cache_dir_base=cache_dir_base,
                     load_fact=load_fact,
                     fact_dir=fact_dir,
                     fact_versions=fact_versions,
                     fact_proj_roots=fact_proj_roots,
                     restrict_fact=restrict_fact,
                     fact_for_changes=fact_for_changes,
                     fact_for_mapping=fact_for_mapping,
                     fact_for_ast=fact_for_ast,
                     fact_into_virtuoso=fact_into_virtuoso,
                     fact_into_directory=fact_into_directory,
                     fact_size_thresh=fact_size_thresh,
                     fact_encoding=fact_encoding,
                     fact_hash_algo=fact_hash_algo,
                     fact_no_compress=fact_no_compress,
                     dumpccs=dumpccs,
                     check=check,
                     aggressive=aggressive,
                     keep_filtered_temp=keep_filtered_temp,
                     local_cache_name=local_cache_name,
                     dump_delta=dump_delta,
                     fact_for_delta=fact_for_delta,
                     keep_going=keep_going,
                     quiet=quiet,
                     )

            t0 = time.time() - st0

            c = r['cost']
            m = r['nmappings']

            logger.info(f'"{file1}" - "{file2}": CMR={c}/{m} ({t0:.2f}s)')

            fvs0 = []
            fvs1 = []
            if len(fact_versions) == 2 and load_fact:
                fvs0 = [fact_versions[0]]
                fvs1 = [fact_versions[1]]

            fpr0 = []
            fpr1 = []
            if len(fact_proj_roots) == 2 and load_fact:
                fpr0 = [fact_proj_roots[0]]
                fpr1 = [fact_proj_roots[1]]

            count_opts['fact_versions'] = fvs0
            count_opts['fact_proj_roots'] = fpr0

            if no_node_count:
                d_nnodes1 = 0
            else:
                d_nnodes1 = count_nodes([file1], **count_opts)

            count_opts['fact_versions'] = fvs1
            count_opts['fact_proj_roots'] = fpr1

            if no_node_count:
                d_nnodes2 = 0
            else:
                d_nnodes2 = count_nodes([file2], **count_opts)

            nnodes1 += d_nnodes1
            nnodes2 += d_nnodes2
            nnodes += d_nnodes1 + d_nnodes2

            cost += c
            nmappings += m
            nrelabels += r['nrelabels']
            nmoves += r['nmoves']
            nmovrels += r['nmovrels']

    except Exception as e:
        logger.warning(f'{e}')

    t = time.time() - st_time

    m = t / 60.0

    ncomp = len(modified)

    logger.info(f'"{dir1}" - "{dir2}" --> {ncomp} comparisons in {t:.2f} sec. ({m:.2f} min.)')

    res = {'cost': cost,
           'ncomparisons': ncomp,
           'nmappings': nmappings,
           'nnodes1': nnodes1,
           'nnodes2': nnodes2,
           'nnodes': nnodes,
           'nrelabels': nrelabels,
           'nmoves': nmoves,
           'nmovrels': nmovrels,

           'modified': modified,
           'renamed': renamed,
           'moved': moved,
           'added': added,
           'removed': removed,
           'copied': copied,
           'glued': glued,
           }

    if line_sim and line_sim_count > 0:
        res['line_sim'] = line_sim_sum / line_sim_count

    with open(cache_dir_info_json, 'w') as f:
        json.dump(res, f)

    return res


#####

def test_diff_dirs():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='compare directories',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('dir1', type=str)
    parser.add_argument('dir2', type=str)

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug output')

    parser.add_argument('--sim', dest='use_sim', action='store_true',
                        help='track files relying on the similarity between them')

    parser.add_argument('--sim-thresh', dest='sim_thresh', metavar='R',
                        type=float, default=0.7,
                        help='set similarity threshold')

    parser.add_argument('-m', '--mode', dest='mode', metavar='MODE',
                        choices=['diffast'], default='diffast',
                        help='set mode to MODE')

    args = parser.parse_args()

    log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)

    mode = args.mode

    logger.info(f'mode: "{mode}"')

    diff = None

    if mode == 'diffast':
        diff = diffast
    else:
        logger.error(f'illegal mode: "{mode}"')

    res = diff_dirs(diff, args.dir1, args.dir2,
                    use_sim=args.use_sim,
                    sim_thresh=args.sim_thresh)

    cost = res['cost']
    ncomps = res['ncomparisons']
    nmappings = res['nmappings']

    logger.info(f'TOTAL COST        : {cost}')
    logger.info(f'TOTAL MAPPING SIZE: {nmappings}')
    logger.info(f'# of comparisons  : {ncomps}')


def test_count_nodes():
    c = count_nodes([sys.argv[1]])
    print(c)


def test_get_cache_dir():
    file1 = sys.argv[1]
    file2 = sys.argv[2]
    wdir = sys.argv[3]
    print(get_cache_dir(file1, file2, wdir))


if __name__ == '__main__':
    test_diff_dirs()
    # test_count_nodes()
    # test_get_cache_dir()
