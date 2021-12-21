#!/usr/bin/env python3


'''
  diffinfo.py

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

import re
import os
import gzip
import logging

from .fragment import Fragment

logger = logging.getLogger()

read_delete_insert_info_size_threshold = 4


excl_L_pat = re.compile(r'\) \[')
excl_R_pat = re.compile(r'(?P<list>.*)\]')
num_pat = re.compile(r'(?P<num>[0-9]+);?')


def get_excluded(s):
    result = []
    li = excl_L_pat.finditer(s)
    start = -1
    for m in li:
        start = m.end()
    if start > 0:
        m = excl_R_pat.search(s, start)
        if m:
            s = m.group('list')
            result = [int(x) for x in num_pat.findall(s)]

    return result


named_node_pat_s = r'\((?P<size>[0-9]+)\) \(([0-9]+):(?P<gnid>[0-9]+)\)c:(?P<kind>.*) name=\'(?P<name>.*)\'(?P<rest>.*)\((?P<loc>[0-9]+L.*)\)(?P<exc>.*)\((?P<elems>.*)\)$'
pat_s = r'\((?P<size>[0-9]+)\) \(([0-9]+):(?P<gnid>[0-9]+)\)c:(?P<kind>.*)\((?P<loc>[0-9]+L.*)\)(?P<exc>.*)\((?P<elems>.*)\)$'
named_node_insert_pat = re.compile('INSERT' + named_node_pat_s)
insert_pat = re.compile('INSERT' + pat_s)
named_node_delete_pat = re.compile('DELETE' + named_node_pat_s)
delete_pat = re.compile('DELETE' + pat_s)


def read_delete_insert_info(fname):
    logger.info('reading "{}"'.format(fname))
    deletes = []
    inserts = []
    try:
        f = open(fname)
        for line in f:
            line = line.rstrip()

            m = named_node_delete_pat.search(line)
            if m:
                size = int(m.group('size'))
                name = m.group('name')
                if name and size > read_delete_insert_info_size_threshold:
                    excluded = get_excluded(m.group('exc'))
                    elems = Fragment(m.group('elems'))
                    rest = m.group('rest')
                    loc = m.group('loc')
                    kind = m.group('kind') + '|' + rest
                    gnid = int(m.group('gnid'))
                    r = {
                        'loc': loc,
                        'size': size,
                        'kind': kind,
                        'name': name,
                        'gnid': gnid,
                        'excluded': excluded,
                        'elems': elems
                    }
                    deletes.append(r)
            else:
                m = delete_pat.search(line)
                if m:
                    size = int(m.group('size'))
                    if size > read_delete_insert_info_size_threshold:
                        kind = m.group('kind')
                        loc = m.group('loc')
                        gnid = int(m.group('gnid'))
                        excluded = get_excluded(m.group('exc'))
                        elems = Fragment(m.group('elems'))
                        r = {
                            'loc': loc,
                            'size': size,
                            'kind': kind,
                            'name': None,
                            'gnid': gnid,
                            'excluded': excluded,
                            'elems': elems
                        }
                        deletes.append(r)

            m = named_node_insert_pat.search(line)
            if m:
                size = int(m.group('size'))
                name = m.group('name')
                if name and size > read_delete_insert_info_size_threshold:
                    excluded = get_excluded(m.group('exc'))
                    elems = Fragment(m.group('elems'))
                    rest = m.group('rest')
                    loc = m.group('loc')
                    kind = m.group('kind') + '|' + rest
                    gnid = int(m.group('gnid'))
                    r = {
                        'loc': loc,
                        'size': size,
                        'kind': kind,
                        'name': name,
                        'gnid': gnid,
                        'excluded': excluded,
                        'elems': elems
                    }
                    inserts.append(r)
            else:
                m = insert_pat.search(line)
                if m:
                    size = int(m.group('size'))
                    if size > read_delete_insert_info_size_threshold:
                        kind = m.group('kind')
                        loc = m.group('loc')
                        gnid = int(m.group('gnid'))
                        excluded = get_excluded(m.group('exc'))
                        elems = Fragment(m.group('elems'))
                        r = {'loc': loc,
                             'size': size,
                             'kind': kind,
                             'name': None,
                             'gnid': gnid,
                             'excluded': excluded,
                             'elems': elems
                             }
                        inserts.append(r)

        f.close()
    except IOError as e:
        logger.warning(str(e))

    return (deletes, inserts)


map_pat = re.compile(r'(?P<kind>R|E)\[#([0-9]+)U:#(?P<gi1>[0-9]+)G\](?P<lab1>.*)\[(?P<loc1>.*)\] -- \[#([0-9]+)U:#(?P<gi2>[0-9]+)G\](?P<lab2>.*)\[(?P<loc2>.*)\]')


def read_map_info(info, swapped=False):
    map_file_not_found = True
    gi_map = []
    relabeled_gis = []
    empty_map = True

    opener = open

    if os.path.exists(info):
        pass
    else:  # maybe compressed
        info = info + '.gz'
        opener = gzip.open

    try:
        f = opener(info)
        map_file_not_found = False
        for line in f:
            m = map_pat.search(line)
            if m:
                empty_map = False
                gi1 = int(m.group('gi1'))
                gi2 = int(m.group('gi2'))
                kind = m.group('kind')
                # lab1 = (m.group('lab1'))
                # lab2 = (m.group('lab2'))
                # loc1 = (m.group('loc1'))
                # loc2 = (m.group('loc2'))

                if swapped:
                    gi_map.append((gi2, gi1))
                    if kind == 'R':
                        relabeled_gis.append(gi2)
                else:
                    gi_map.append((gi1, gi2))
                    if kind == 'R':
                        relabeled_gis.append(gi1)

        f.close()

    except BaseException as e:
        logger.warning(str(e))
        if map_file_not_found:
            gi_map = None
            relabeled_gis = None

    if empty_map:
        logger.warning('empty map: "{}"'.format(info))

    return (gi_map, relabeled_gis)


lmap_pat = re.compile(r'(R|E)\[(?P<loc1>[0-9]+L.*)\].* -- .*\[(?P<loc2>[0-9]+L.*)\]')


def read_lmap_info(info, swapped=False):
    result = []
    try:
        f = open(info)
        for line in f:
            m = lmap_pat.search(line)
            if m:
                loc1 = m.group('loc1')
                loc2 = m.group('loc2')
                if swapped:
                    result.append((loc2, loc1))
                else:
                    result.append((loc1, loc2))
        f.close()
    except Exception as e:
        logger.warning(str(e))

    return result


def test(mapfile):
    (gi_map, relabeled_gis) = read_map_info('map.gz')
    print('gindex map read: size={}'.format(len(gi_map)))
    print('{} relabeled gindexes found'.format(len(relabeled_gis)))


if __name__ == '__main__':
    test('map.gz')
