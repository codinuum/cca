#!/usr/bin/env python3

'''
  plain_patch.py

  Copyright 2018-2020 Chiba Institute of Technology

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
import sys
import re
import filecmp
import difflib
import time
import random
import logging

from .common import setup_logger

logger = logging.getLogger()

EXTS = ['.java', '.jj', '.jjt', '.properties']

HEAD_PAT0 = re.compile(r'^--- (?P<path>[A-Za-z0-9._/-]+).*$')
HEAD_PAT1 = re.compile(r'^\+\+\+ (?P<path>[A-Za-z0-9._/-]+).*$')
HUNK_HEAD_PAT = re.compile(r'^@@ -(?P<sl0>[0-9]+),(?P<c0>[0-9]+) \+(?P<sl1>[0-9]+),(?P<c1>[0-9]+) @@$')


def get_head0(s):
    h = None
    m = HEAD_PAT0.match(s)
    if m:
        path = m.group('path')
        h = (s, path)
    return h


def get_head1(s):
    h = None
    m = HEAD_PAT1.match(s)
    if m:
        path = m.group('path')
        h = (s, path)
    return h


def get_hunk_head(s):
    rs = None
    m = HUNK_HEAD_PAT.match(s)
    if m:
        sl0 = int(m.group('sl0'))
        c0 = int(m.group('c0'))
        sl1 = int(m.group('sl1'))
        c1 = int(m.group('c1'))
        rs = (s, (sl0, c0, sl1, c1))
    return rs


def is_src(f):
    return any([f.endswith(ext) for ext in EXTS])


class IdGenerator(object):
    def __init__(self):
        self._count = 0

    def gen(self):
        i = self._count
        self._count += 1
        return i


class Hunk(object):
    def __init__(self, head_ranges):
        head, ranges = head_ranges
        self.head = head
        self.ranges = ranges
        self._lines = []

    def __str__(self):
        return '<HUNK:%s(%s)->%s(%s)>' % self.ranges

    def add_line(self, li):
        self._lines.append(li)

    def dump(self, out):
        out.write(self.head)
        for ln in self._lines:
            out.write(ln)


class Header(object):
    def __init__(self, head1_path1, head2_path2):
        head1, path1 = head1_path1
        head2, path2 = head2_path2
        self.head1 = head1
        self.head2 = head2
        self.path1 = path1
        self.path2 = path2

    def __str__(self):
        return '<HEADER:%s>' % self.path1

    def dump(self, out):
        out.write(self.head1)
        out.write(self.head2)


class Patch(object):
    def __init__(self, dpath1, dpath2, filt=None, shuffle=0):
        self._idgen = IdGenerator()
        self._hunk_tbl = {}  # hid -> hunk
        self._header_tbl = {}  # hunk -> header
        self._dpath1 = dpath1
        self._dpath2 = dpath2

        self._filt = lambda x: True
        if filt:
            self._filt = filt

        self.compare_dirs(dpath1, dpath2)

        if shuffle:
            permutation = range(len(self))
            for i in range(shuffle):
                random.shuffle(permutation)
            logger.info('permutation=%s' % permutation)
            tbl = {}
            for (hid, hunk) in self._hunk_tbl.items():
                tbl[permutation[hid]] = hunk
            self._hunk_tbl = tbl

    def __len__(self):
        return len(self._hunk_tbl.keys())

    def __str__(self):
        return '<PATCH:%d>' % len(self._hunk_tbl.keys())

    def get_hunk_ids(self):
        return self._hunk_tbl.keys()

    def dump(self, hids=None, out=sys.stdout):
        tbl = {}  # header -> hunk list

        if hids is None:
            hids = self._hunk_tbl.keys()

        for hid in hids:
            hunk = self._hunk_tbl[hid]
            header = self._header_tbl[hunk]
            try:
                li = tbl[header]
            except KeyError:
                li = []
                tbl[header] = li
            li.append(hunk)

        for (header, hunks) in tbl.items():
            header.dump(out)
            for hunk in sorted(hunks, key=lambda h: h.ranges):
                hunk.dump(out)

    def gen_id(self):
        return self._idgen.gen()

    def get_hunk(self, hid):
        return self._hunk_tbl[hid]

    def get_header(self, hunk):
        return self._header_tbl[hunk]

    def reg_hunk(self, header, hunk):
        hid = self.gen_id()
        self._hunk_tbl[hid] = hunk
        self._header_tbl[hunk] = header

    def compare_dirs(self, d1, d2):
        logger.info('comparing %s with %s' % (d1, d2))
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
                    if self._filt(p):
                        logger.debug('R %s' % p)
                        removed_files.append(p)

                elif os.path.isdir(p):
                    logger.debug('R %s' % p)
                    removed_dirs.append(p)

            for f in dc.right_only:
                p = os.path.join(dc.right, f)
                if is_src(f):
                    if self._filt(p):
                        logger.debug('A %s' % p)
                        added_files.append(p)

                elif os.path.isdir(p):
                    logger.debug('A %s' % p)
                    added_dirs.append(p)

            for f in dc.diff_files:
                if is_src(f):
                    p1 = os.path.join(dc.left, f)
                    p2 = os.path.join(dc.right, f)
                    if self._filt(p1) and self._filt(p2):
                        logger.debug('M %s' % p1)
                        modified_files.append((p1, p2))

            for subd in dc.subdirs.values():
                scan(subd)

        scan(dcmp)

        for f1 in removed_files:
            self.compare_files(f1, None)

        for f2 in added_files:
            self.compare_files(None, f2)

        for d1 in removed_dirs:
            self.scan_files(d1, self.reg_file_del_patch)

        for d2 in added_dirs:
            self.scan_files(d2, self.reg_file_ins_patch)

        for (f1, f2) in modified_files:
            self.compare_files(f1, f2)

    def scan_files(self, x, f):
        for (d, dns, ns) in os.walk(x):
            for n in ns:
                p = os.path.join(d, n)
                if self._filt(p):
                    f(p)

    def reg_file_del_patch(self, path):
        date = time.ctime()  # time.ctime(os.stat(path).st_mtime)
        with open(path, 'U') as f:
            lines = f.readlines()
            count = len(lines)
            p = os.path.relpath(path, self._dpath1)
            header = Header(('--- %s %s\n' % (p, date), path),
                            ('+++ /dev/null %s\n' % date, '/dev/null'))
            hunk = Hunk(('@@ -1,%d +0,0 @@\n' % count, (1, count, 0, 0)))
            last_line = None
            for line in lines:
                last_line = line
                hunk.add_line('-'+line)

            if last_line and not last_line.endswith('\n'):
                hunk.add_line('\n\\ No newline at end of file\n')

            hid = self.gen_id()
            self._hunk_tbl[hid] = hunk
            self._header_tbl[hunk] = header

    def reg_file_ins_patch(self, path):
        date = time.ctime()  # time.ctime(os.stat(path).st_mtime)
        with open(path, 'U') as f:
            lines = f.readlines()
            count = len(lines)
            p = os.path.relpath(path, self._dpath2)
            header = Header(('--- /dev/null %s\n' % date, '/dev/null'),
                            ('+++ %s %s\n' % (p, date), path))
            hunk = Hunk(('@@ -0,0 +1,%d @@\n' % count, (0, 0, 1, count)))
            last_line = None
            for line in lines:
                last_line = line
                hunk.add_line('+'+line)

            if last_line and not last_line.endswith('\n'):
                hunk.add_line('\n\\ No newline at end of file\n')

            hid = self.gen_id()
            self._hunk_tbl[hid] = hunk
            self._header_tbl[hunk] = header

    def compare_files(self, file1, file2):
        logger.info('comparing %s with %s' % (file1, file2))

        if file1 and file2 is None:
            self.reg_file_del_patch(file1)

        elif file1 is None and file2:
            self.reg_file_ins_patch(file2)

        elif file1 and file2:
            date1 = time.ctime()  # time.ctime(os.stat(file1).st_mtime)
            date2 = time.ctime()  # time.ctime(os.stat(file2).st_mtime)

            with open(file1, 'U') as f1:
                lines1 = f1.readlines()

            with open(file2, 'U') as f2:
                lines2 = f2.readlines()

            p1 = os.path.relpath(file1, self._dpath1)
            p2 = os.path.relpath(file2, self._dpath2)

            dls = difflib.unified_diff(lines1, lines2, p1, p2, date1, date2)

            head0 = None
            head1 = None
            hunk_head = None

            header = None
            hunk = None

            for dl in dls:
                if head0 is None:
                    head0 = get_head0(dl)

                if head1 is None:
                    head1 = get_head1(dl)

                hunk_head = get_hunk_head(dl)

                if head0:
                    logger.debug('HEAD0:%s' % (head0,))
                if head1:
                    logger.debug('HEAD1:%s' % (head1,))
                if hunk_head:
                    logger.debug('HUNK_HEAD:%s' % (hunk_head,))

                if hunk and not hunk_head:
                    if not dl.endswith('\n'):
                        dl += '\n\\ No newline at end of file\n'
                    hunk.add_line(dl)

                if head0 and head1:
                    header = Header(head0, head1)
                    head0 = None
                    head1 = None

                if header and hunk_head:
                    hunk = Hunk(hunk_head)
                    hunk_head = None
                    self.reg_hunk(header, hunk)

                logger.debug(dl)

            logger.debug(header)


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='decompose patch',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')
    parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                        help='enable verbose printing')

    parser.add_argument('dir1', type=str, help='base directory')
    parser.add_argument('dir2', type=str, help='modified directory')

    args = parser.parse_args()

    log_level = logging.WARNING
    if args.verbose:
        log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)

    patch = Patch(args.dir1, args.dir2)

    hids = patch.get_hunk_ids()
    print('%d hunks generated' % (len(hids)))

    for hid in hids:
        print('*** Hunk ID: %s' % hid)
        patch.dump([hid])


if __name__ == '__main__':
    main()
