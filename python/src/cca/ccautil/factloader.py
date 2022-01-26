#!/usr/bin/env python3


'''
  factloader.py

  Copyright 2012-2021 Codinuum Software Lab <https://codinuum.com>

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
import gzip
import tempfile
import shutil
import logging

from . import diffts

logger = logging.getLogger()

DEFAULT_TEMP_FILE_SIZE = 3000000000


def make_factbase_dir(basedir, proj_id):
    dname = 'factbase-' + proj_id
    path = os.path.realpath(os.path.join(basedir, dname))
    return path


def _scan_dir(lvl, dpath, pat, f):
    if os.path.isdir(dpath):
        mems = os.listdir(dpath)
        nmems = len(mems)
        count = 0
        for x in mems:
            count += 1
            p = os.path.join(dpath, x)
            if os.path.isdir(p):
                _scan_dir(lvl + 1, p, pat, f)
            else:
                matched = False
                if type(pat) == str:
                    matched = x == pat
                else:
                    matched = pat.match(x)

                if matched:
                    f(p)

            if lvl == 0:
                per = float(count) / float(nmems) * 100
                logger.info('{:.2f}% ({}/{}) completed'
                            .format(per, count, nmems))


def scan_dir(dpath, pat, f):
    _scan_dir(0, dpath, pat, f)


class FactLoader(object):
    def __init__(self, d):
        self.fact_dir = d
        if not os.path.isdir(d):
            os.makedirs(d)

    def load(self, path):
        logger.warning('not implemented')


class DefaultFactLoader(FactLoader):
    def load(self, path):
        dest = os.path.join(self.fact_dir, os.path.basename(path))
        shutil.copy(path, dest)


class FactMerger(object):
    def __init__(self, loader, temp_file_size=DEFAULT_TEMP_FILE_SIZE):
        (h, temp) = tempfile.mkstemp()
        os.close(h)
        self._current_temp_file = temp
        logger.info('initial temp file "{}" created'.format(temp))
        self._loader = loader
        self._rotate_count = 0
        self._temp_file_size = temp_file_size

    def load(self):
        current = self._current_temp_file
        if current is not None:
            n = self._loader.load(current)

            if n < 0:
                logger.warning('load failed')
            else:
                os.remove(current)
                logger.info('"{}" removed'.format(current))

            self._current_temp_file = None

    def rotate(self):
        self._rotate_count += 1
        self.load()
        (h, temp) = tempfile.mkstemp()
        os.close(h)
        self._current_temp_file = temp
        logger.info('[{}] new temp file "{}" created'
                    .format(self._rotate_count, temp))

    def get_current_temp_file(self):
        return self._current_temp_file

    def merge(self, infile):
        outfile = self._current_temp_file
        if os.path.getsize(outfile) >= self._temp_file_size:
            self.rotate()
            outfile = self._current_temp_file

        try:
            f_in = gzip.open(infile, 'rb')
            f_out = open(outfile, 'ab')
            f_out.write(f_in.read())
            f_out.close()
            f_in.close()
        except BaseException as e:
            logger.warning('{}: {}'.format(infile, e))


def load(loader, cache_dir_base, temp_file_size=DEFAULT_TEMP_FILE_SIZE):

    logger.info('max temp file size: {}'.format(temp_file_size))

    logger.info('merging fact')
    merger0 = FactMerger(loader, temp_file_size)
    scan_dir(cache_dir_base, diffts.fact_file_name_pat, merger0.merge)
    merger0.load()

    logger.info('merging mapping fact')
    merger1 = FactMerger(loader, temp_file_size)
    scan_dir(cache_dir_base, diffts.mapfact_file_name, merger1.merge)
    merger1.load()

    logger.info('merging change fact')
    merger2 = FactMerger(loader, temp_file_size)
    scan_dir(cache_dir_base, diffts.changefact_file_name, merger2.merge)
    merger2.load()

    logger.info('merging cfg fact')
    merger3 = FactMerger(loader, temp_file_size)
    scan_dir(cache_dir_base, diffts.cfgfact_file_name, merger3.merge)
    merger3.load()


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='load Diff/TS generated fact',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('cache_dir_base', type=str, help='cache dir base')
    parser.add_argument('out_dir', type=str, help='output dir')
    parser.add_argument('--temp-file-size', dest='temp_file_size',
                        default=DEFAULT_TEMP_FILE_SIZE, metavar='N', type=int,
                        help='maximum temp file size (in bytes) for RDF loader')

    args = parser.parse_args()

    loader = DefaultFactLoader(args.out_dir)

    load(loader, args.cache_dir_base, args.temp_file_size)


if __name__ == '__main__':
    main()
