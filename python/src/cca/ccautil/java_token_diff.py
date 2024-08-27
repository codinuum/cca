#!/usr/bin/env python3

'''
  java_token_diff.py

  Copyright 2018-2024 Chiba Institute of Technology

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
import logging

import filecmp
from difflib import SequenceMatcher
from javalang import tokenizer

logger = logging.getLogger()


def is_src(f):
    return f.endswith('.java')


def get_tokens(path):
    toks = []
    try:
        with open(path, 'r') as f:
            for tok in tokenizer.tokenize(f.read()):
                toks.append(tok.value)
    except Exception:
        pass

    seq = []

    while True:
        try:
            tok = toks.pop(0)

            if tok == '.':
                try:
                    nxt = toks.pop(0)
                    r = '.' + nxt
                    if seq:
                        if seq[-1] not in (',', '('):
                            seq[-1] += r
                        else:
                            seq.append(r)
                    else:
                        seq.append(r)

                except IndexError:
                    seq.append(tok)

            elif tok == ',':
                try:
                    nxt = toks.pop(0)
                    if nxt in ('}', ';'):
                        seq.append(nxt)
                    else:
                        seq.append(tok)
                        seq.append(nxt)

                except IndexError:
                    seq.append(tok)

            else:
                seq.append(tok)

        except IndexError:
            break

    return seq


def count_tokens(path):
    c = len(get_tokens(path))
    return c


def get_files(x):
    li = []
    for (d, dns, ns) in os.walk(x):
        for n in ns:
            p = os.path.join(d, n)
            if is_src(p):
                li.append(p)
    return li


def get_pre_context(toks, i):
    if i > 2:
        pre = ' '.join(toks[i-3:i])
    elif i == 2:
        pre = ' '.join(toks[i-2:i])
    elif i == 1:
        pre = ' '.join(toks[i-1:i])
    else:
        pre = ' '.join(toks[0:i])
    return pre


def get_post_context(toks, i):
    post = ' '.join(toks[i:i+5])
    return post


def get_context(toks, i):
    return (get_pre_context(toks, i), get_post_context(toks, i))


def diff_to_str(d, toks1, toks2):
    dels = d['delete']
    repls = d['replace']
    inss = d['insert']

    lines = []

    if dels:
        for ((a, b), _) in dels:
            pre, post = get_pre_context(toks1, a), get_post_context(toks1, b)
            lines.append(f'[DELETE] {a}-{b-1} ({b-a}):\n')
            lines.append(f'  {pre}\n')
            lines.append('- ')
            lines.append(' '.join(toks1[a:b]))
            lines.append('\n')
            lines.append(f'  {post}\n')
    if repls:
        for ((a, b), (a2, b2)) in repls:
            pre, post = get_pre_context(toks1, a), get_post_context(toks1, b)
            lines.append(f'[REPLACE] {a}-{b-1} -> {a2}-{b2-1} ({b-a}->{b2-a2}):\n')
            lines.append(f'  {pre}\n')
            lines.append('- ')
            lines.append(' '.join(toks1[a:b]))
            lines.append('\n-----\n')
            lines.append('+ ')
            lines.append(' '.join(toks2[a2:b2]))
            lines.append('\n')
            lines.append(f'  {post}\n')
    if inss:
        for ((i, _), (a, b)) in inss:
            pre, post = get_context(toks1, i)
            lines.append(f'[INSERT] {i} -> {a}-{b-1} ({b-a}):\n')
            lines.append('  {}\n'.format(pre))
            lines.append('+ ')
            lines.append(' '.join(toks2[a:b]))
            lines.append('\n')
            lines.append(f'  {post}\n')

    s = ''.join(lines)

    return s


def print_diff(d, toks1, toks2):
    print(diff_to_str(d, toks1, toks2))


def size_of_diff(d):
    sz = 0
    for ((i1, i2), _) in d['delete']:
        sz += i2 - i1

    for ((i1, i2), (j1, j2)) in d['replace']:
        sz += i2 - i1 + j2 - j1

    for (_, (j1, j2)) in d['insert']:
        sz += j2 - j1

    return sz


def diff_tokens(toks1, toks2):
    m = SequenceMatcher(isjunk=None, a=toks1, b=toks2)
    d = {'replace': [], 'delete': [], 'insert': []}
    for (tag, i1, i2, j1, j2) in m.get_opcodes():
        if tag != 'equal':
            d[tag].append(((i1, i2), (j1, j2)))
    d['sim'] = m.ratio()
    nm = 0
    for nt in m.get_matching_blocks():
        nm += nt.size
    d['nmatches'] = nm
    return d


def is_equivalent_file(path1, path2):
    if filecmp.cmp(path1, path2, shallow=False):
        logger.info('same files')
        return True

    logger.debug(f'comparing {path1} with {path2}')

    toks1 = get_tokens(path1)
    toks2 = get_tokens(path2)
    b = toks1 == toks2
    return b


def all_different(paths):
    n = len(paths)
    for i in range(n-1):
        for j in range(i+1, n):
            if filecmp.cmp(paths[i], paths[j], shallow=False):
                logger.info(f'same files: {paths[i]} {paths[j]}')
                return False

    toks_list = [None for _ in paths]

    for i in range(n-1):
        for j in range(i+1, n):
            if toks_list[i] is None:
                toks_list[i] = get_tokens(paths[i])
            if toks_list[j] is None:
                toks_list[j] = get_tokens(paths[j])
            if toks_list[i] is toks_list[j]:
                logger.info(f'equivalent files: {paths[i]} {paths[j]}')
                return False

    return True


def compare_files(path1, path2, simple=False):
    if filecmp.cmp(path1, path2, shallow=False):
        logger.info('same files')
        return {'count': 0, 'diff': '', 'sim': 1.0}
    elif simple:
        logger.info('different files')
        return {}

    logger.debug(f'comparing {path1} with {path2}')

    toks1 = get_tokens(path1)
    toks2 = get_tokens(path2)
    d = diff_tokens(toks1, toks2)
    c = size_of_diff(d)
    s = diff_to_str(d, toks1, toks2)
    sim = d['sim']
    nm = d['nmatches']
    dist = float(c) / (float(nm) if nm > 0 else 1.0)
    ret = {'count': c, 'diff': s, 'sim': sim, 'dist': dist}
    return ret


def compare_dirs(d1, d2, simple=False):
    # print('comparing {} with {}'.format(d1, d2))

    dcmp = filecmp.dircmp(d1, d2)
    removed_files = []
    added_files = []
    modified_files = []

    removed_dirs = []
    added_dirs = []

    def scan(dc):
        for f in dc.left_only:
            p = os.path.join(dc.left, f)
            if is_src(f):
                removed_files.append(p)

            elif os.path.isdir(p):
                removed_dirs.append(p)

        for f in dc.right_only:
            p = os.path.join(dc.right, f)
            if is_src(f):
                added_files.append(p)

            elif os.path.isdir(p):
                added_dirs.append(p)

        for f in dc.diff_files:
            if is_src(f):
                p1 = os.path.join(dc.left, f)
                p2 = os.path.join(dc.right, f)
                modified_files.append((p1, p2))

        for subd in dc.subdirs.values():
            scan(subd)

    scan(dcmp)

    count = 0

    for f in removed_files:
        count += count_tokens(f)

    for f in added_files:
        count += count_tokens(f)

    for d in removed_dirs:
        for f in get_files(d):
            count += count_tokens(f)

    for d in added_dirs:
        for f in get_files(d):
            count += count_tokens(f)

    for (f1, f2) in modified_files:
        r = compare_files(f1, f2, simple=simple)
        if r:
            count += r['count']

    return count


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='compute size of token sequence delta',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('path1', type=str)
    parser.add_argument('path2', type=str)

    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='enable verbose printing')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('-s', '--simple', dest='simple', action='store_true',
                        help='only checks if file1 is equivalent to file2')

    args = parser.parse_args()

    log_level = logging.WARNING
    if args.verbose:
        log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    logging.basicConfig(format='[%(levelname)s][%(funcName)s] %(message)s',
                        level=log_level)

    c = None

    if os.path.isfile(args.path1) and os.path.isfile(args.path2):
        r = compare_files(args.path1, args.path2, simple=args.simple)
        if r:
            d = r['diff']
            if d:
                logger.debug(f'differences:\n{d}')
            c = r['count']

    elif os.path.isdir(args.path1) and os.path.isdir(args.path2):
        c = compare_dirs(args.path1, args.path2, simple=args.simple)

    print(c)


if __name__ == '__main__':
    main()
