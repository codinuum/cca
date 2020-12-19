#!/usr/bin/env python3


'''
  verdiff.py

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
import re
import gzip 
import tempfile
import logging

import pathsetup
import tp
import diffts
import srcdiff
import project
import cca_options
import AST
from factloader import make_factbase_dir, scan_dir, FactMerger
from factloader import DEFAULT_TEMP_FILE_SIZE, DefaultFactLoader
from factextractor import Enc, HashAlgo
import cca_factextractor
from common import setup_logger

logger = logging.getLogger()


diff = srcdiff.diffast


class TaskPool(tp.base):

    def __init__(self, basedir, wdir, conf, clear_cache=True,
                 load_fact=False, fact_dir=None,
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
                 line_sim=False,
                 local_cache_name=None,
                 dump_delta=False,
                 fact_for_delta=False,
                 keep_going=False,
                 use_sim=False,
                 sim_thresh=0.7,
    ):

        tp.base.__init__(self, working_dir=wdir, clear_cache=clear_cache)
        self.basedir             = basedir
        self.__conf              = conf
        self.load_fact           = load_fact
        self.fact_dir            = fact_dir
        self.ignore_unmodified   = ignore_unmodified
        self.restrict_fact       = restrict_fact
        self.fact_for_changes    = fact_for_changes
        self.fact_for_mapping    = fact_for_mapping
        self.fact_for_ast        = fact_for_ast
        self.fact_into_virtuoso  = fact_into_virtuoso
        self.fact_into_directory = fact_into_directory
        self.fact_size_thresh    = fact_size_thresh
        self.fact_for_cfg        = fact_for_cfg
        self.fact_encoding       = fact_encoding
        self.fact_hash_algo      = fact_hash_algo
        self.line_sim            = line_sim
        self.local_cache_name    = local_cache_name
        self.dump_delta          = dump_delta
        self.fact_for_delta      = fact_for_delta
        self.keep_going          = keep_going
        self.use_sim             = use_sim
        self.sim_thresh          = sim_thresh

        self.cache_dir_base = os.path.abspath(os.path.join(self.basedir, 'work.diffts', self.__conf.proj_id))
        if self.load_fact:
            self.cache_dir_base += '.fact'

        logger.info('load_fact: {}'.format(self.load_fact))
        logger.info('fact_dir: {}'.format(self.fact_dir))
        logger.info('ignore_unmodified: {}'.format(self.ignore_unmodified))
        logger.info('restrict_fact: {}'.format(self.restrict_fact))
        logger.info('fact_for_changes: {}'.format(self.fact_for_changes))
        logger.info('fact_for_mapping: {}'.format(self.fact_for_mapping))
        logger.info('fact_for_ast: {}'.format(self.fact_for_ast))
        logger.info('fact_into_virtuoso: {}'.format(self.fact_into_virtuoso))
        logger.info('fact_into_directory: {}'.format(self.fact_into_directory))
        logger.info('fact_size_thresh: {}'.format(self.fact_size_thresh))
        logger.info('fact_for_cfg: {}'.format(self.fact_for_cfg))
        logger.info('fact_encoding: {}'.format(self.fact_encoding))
        logger.info('fact_hash_algo: {}'.format(self.fact_hash_algo))

        if local_cache_name:
            logger.info('local_cache_name: {}'.format(self.local_cache_name))

        logger.info('dump_delta: {}'.format(self.dump_delta))
        logger.info('fact_for_delta: {}'.format(self.fact_for_delta))
        logger.info('keep_going: {}'.format(self.keep_going))
        logger.info('use_sim: {}'.format(self.use_sim))
        logger.info('sim_thresh: {}'.format(self.sim_thresh))


    def mkver_for_fact(self, idx):
        return self.__conf.mkver_for_fact(idx)

    def mkver_for_fact_by_name(self, name):
        return self.__conf.mkver_for_fact_by_name(name)

    def gen_tasks(self):
        tasks = []

        if self.__conf.vpairs:
            for (v1, v2) in self.__conf.vpairs:
                ver1 = self.__conf.get_long_name(v1)
                ver2 = self.__conf.get_long_name(v2)
                tasks.append((self.mkver_for_fact_by_name(v1), self.mkver_for_fact_by_name(v2), ver1, ver2))

        else:
            vers = self.__conf.versions
            for i in range(self.__conf.nversions - 1):
                tasks.append((self.mkver_for_fact(i), self.mkver_for_fact(i+1), vers[i], vers[i+1]))

        return tasks

    def task(self, args):
        (v1, v2, ver1, ver2) = args

        proj_id = self.__conf.proj_id

        gfn1 = AST.mkgensrcsinfofile(self.basedir, proj_id, ver1)
        generated1 = srcdiff.read_stat1(gfn1)
        gfn2 = AST.mkgensrcsinfofile(self.basedir, proj_id, ver2)
        generated2 = srcdiff.read_stat1(gfn2)


        dir1 = self.__conf.get_ver_dir(ver1)
        dir2 = self.__conf.get_ver_dir(ver2)

        if not os.path.exists(dir1):
            logger.error('no such file or directory: {}'.format(dir1))

        if not os.path.exists(dir2):
            logger.error('no such file or directory: {}'.format(dir2))

        if self.__conf.single_file_mode:
            r = diff(dir1, dir2, usecache=(not self.clear_cache),
                     cache_dir_base=self.cache_dir_base,
                     load_fact=self.load_fact,
                     fact_dir=self.fact_dir,
                     fact_versions=[v1, v2],
                     fact_proj_roots=[dir1, dir2],
                     restrict_fact=self.restrict_fact,
                     fact_for_changes=self.fact_for_changes,
                     fact_for_mapping=self.fact_for_mapping,
                     fact_for_ast=self.fact_for_ast,
                     fact_into_virtuoso=self.fact_into_virtuoso,
                     fact_into_directory=self.fact_into_directory,
                     fact_size_thresh=self.fact_size_thresh,
                     fact_encoding=self.fact_encoding,
                     fact_hash_algo=self.fact_hash_algo,
                     local_cache_name=self.local_cache_name,
                     dump_delta=self.dump_delta,
                     fact_for_delta=self.fact_for_delta,
                     keep_going=self.keep_going,
            )

        else:
            r = srcdiff.diff_dirs(diff, dir1, dir2, usecache=(not self.clear_cache),
                                  include=self.__conf.include,
                                  cache_dir_base=self.cache_dir_base,
                                  ignore1=generated1,
                                  ignore2=generated2,
                                  load_fact=self.load_fact,
                                  fact_dir=self.fact_dir,
                                  fact_versions=[v1, v2],
                                  fact_proj=proj_id,
                                  fact_proj_roots=[dir1, dir2],
                                  ignore_unmodified=self.ignore_unmodified,
                                  restrict_fact=self.restrict_fact,
                                  fact_for_changes=self.fact_for_changes,
                                  fact_for_mapping=self.fact_for_mapping,
                                  fact_for_ast=self.fact_for_ast,
                                  fact_into_virtuoso=self.fact_into_virtuoso,
                                  fact_into_directory=self.fact_into_directory,
                                  fact_size_thresh=self.fact_size_thresh,
                                  fact_for_cfg=self.fact_for_cfg,
                                  fact_encoding=self.fact_encoding,
                                  fact_hash_algo=self.fact_hash_algo,
                                  line_sim=self.line_sim,
                                  local_cache_name=self.local_cache_name,
                                  dump_delta=self.dump_delta,
                                  fact_for_delta=self.fact_for_delta,
                                  keep_going=self.keep_going,
                                  use_sim=self.use_sim,
                                  sim_thresh=self.sim_thresh,
            )

        cost      = r['cost']
        nmappings = r['nmappings']
        nrelabels = r['nrelabels']

        try:
            nnodes1   = r['nnodes1']
            nnodes2   = r['nnodes2']
            nnodes    = r['nnodes']
        except KeyError:
            logger.info('failed to get total number of nodes')
            nnodes1 = srcdiff.count_nodes([dir1])
            nnodes2 = srcdiff.count_nodes([dir2])
            nnodes   = nnodes1 + nnodes2

        dist = 0
        sim = 0
        if nmappings > 0:
            dist = float(cost) / float(nmappings)
        if nnodes > 0:
            sim  = float(2 * (nmappings - nrelabels) + nrelabels) / float(nnodes)

        lsim = '-'
        if self.line_sim:
            lsim = str(r['line_sim'])

        return (ver1, ver2, nnodes1, nnodes2, str(dist), str(sim), lsim)


    def load(self, temp_file_size=DEFAULT_TEMP_FILE_SIZE):
        if self.load_fact and self.fact_into_virtuoso == '' and self.fact_into_directory == '':

            logger.info('max temp file size: {}'.format(temp_file_size))

            loader = DefaultFactLoader(self.fact_dir)

            logger.info('merging fact')
            merger0 = FactMerger(loader, temp_file_size)
            scan_dir(self.cache_dir_base, diffts.fact_file_name_pat, merger0.merge)
            merger0.load()

            logger.info('merging mapping fact')
            merger1 = FactMerger(loader, temp_file_size)
            scan_dir(self.cache_dir_base, diffts.mapfact_file_name, merger1.merge)
            merger1.load()

            logger.info('merging change fact')
            merger2 = FactMerger(loader, temp_file_size)
            scan_dir(self.cache_dir_base, diffts.changefact_file_name, merger2.merge)
            merger2.load()

            logger.info('merging cfg fact')
            merger3 = FactMerger(loader, temp_file_size)
            scan_dir(self.cache_dir_base, diffts.cfgfact_file_name, merger3.merge)
            merger3.load()


def compute(load_fact=False,
            ignore_unmodified=False,
            restrict_fact=False,
            fact_for_changes=False,
            fact_for_mapping=False,
            fact_for_ast=False,
            fact_for_delta=False,
            fact_into_virtuoso='',
            fact_into_directory='',
            fact_size_thresh=diffts.DEFAULT_FACT_SIZE_THRESH,
            fact_for_cfg=False,
            fact_encoding=Enc.FDLCO,
            fact_hash_algo=HashAlgo.MD5,
            line_sim=False):

    aparser = cca_factextractor.OptionParser()

    argparser = aparser._get_parser()

    argparser.add_argument('--fact', action='store_true', dest='load_fact',
                           default=load_fact,
                           help='output fact triples')
    argparser.add_argument('--ignore-unmodified', dest='ignore_unmodified',
                           default=False, action='store_true',
                           help='ignore unmodified files (default=%default)')
    argparser.add_argument('--keep-going', dest='keep_going', default=False, action='store_true',
                           help='ignore parse errors (default=%default)')
    argparser.add_argument('--sim', dest='use_sim', default=False, action='store_true',
                           help='track files relying on the similarity between them (default=%default)')
    argparser.add_argument('--sim-thresh', type=float, dest='sim_thresh',
                           default=0.7, metavar='R',
                           help='set similarity threshold (default=%default)')
    argparser.add_argument('--dump-delta', dest='dump_delta',
                           default=False, action='store_true',
                           help='dump delta (default=%default)')
    argparser.add_argument('--delta', action='store_true', dest='fact_for_delta',
                           default=fact_for_delta,
                           help='output fact triples for delta')
    argparser.add_argument('--restrict', action='store_true', dest='restrict_fact',
                           default=restrict_fact,
                           help='restrict fact generation')
    argparser.add_argument('--changes', action='store_true', dest='fact_for_changes',
                           default=fact_for_changes,
                           help='output fact triples for changes')
    argparser.add_argument('--mapping', action='store_true', dest='fact_for_mapping',
                           default=fact_for_mapping,
                           help='output fact triples for mapping')
    argparser.add_argument('--ast', action='store_true', dest='fact_for_ast',
                           default=fact_for_ast,
                           help='output fact triples for AST')
    argparser.add_argument('--cfg', action='store_true', dest='fact_for_cfg',
                           default=fact_for_cfg,
                           help='output fact triples for CFG (generated with coccinelle)')
    argparser.add_argument('--into-virtuoso', type=str, dest='fact_into_virtuoso',
                           default='', metavar='URI',
                           help='dump triples into graph URI in virtuoso (default=%default)')
    argparser.add_argument('--fact-size-thresh', type=int, dest='fact_size_thresh',
                           default=diffts.DEFAULT_FACT_SIZE_THRESH, metavar='N',
                           help='maximum number of facts in a file (default=%default)')
    argparser.add_argument('--temp-file-size', type=int, dest='temp_file_size',
                           default=DEFAULT_TEMP_FILE_SIZE, metavar='N',
                           help='maximum temp file size (in bytes) for RDF loader (default=%default)')



    (args, proj_id, working_dir, argparser) = aparser.get()

    log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)

    load_fact = args.load_fact
    ignore_unmodified = args.ignore_unmodified
    restrict_fact = args.restrict_fact
    fact_for_changes = args.fact_for_changes
    fact_for_mapping = args.fact_for_mapping
    fact_for_ast = args.fact_for_ast
    fact_for_cfg = args.fact_for_cfg
    fact_into_virtuoso = args.fact_into_virtuoso
    fact_into_directory = args.fact_into_directory
    fact_size_thresh = args.fact_size_thresh
    fact_encoding = args.enc
    fact_hash_algo = args.algo

    conf = project.get_conf(proj_id)

#    if args.engine == 'diffast':
#        diff = srcdiff.diffast

    fact_dir = None
    if load_fact:
        fact_dir = make_factbase_dir(args.basedir, proj_id)

    if args.command == 'work':
        logger.info('worker id: "{}"'.format(args.wid))
        pool = TaskPool(args.basedir, working_dir, conf, False, 
                        load_fact, fact_dir, ignore_unmodified, restrict_fact, 
                        fact_for_changes, fact_for_mapping, fact_for_ast, 
                        fact_into_virtuoso, fact_into_directory, fact_size_thresh,
                        fact_for_cfg, fact_encoding, fact_hash_algo,
                        line_sim, #local_cache_name=args.wid,
                        dump_delta=args.dump_delta,
                        fact_for_delta=args.fact_for_delta,
                        keep_going=args.keep_going,
                        use_sim=args.use_sim,
                        sim_thresh=args.sim_thresh,
        )
        pool.watch_tasks(args.wid)

    elif args.command == 'generate':
        pool = TaskPool(args.basedir, working_dir, conf, True, 
                        load_fact, fact_dir, ignore_unmodified, restrict_fact, 
                        fact_for_changes, fact_for_mapping, fact_for_ast, 
                        fact_into_virtuoso, fact_into_directory, fact_size_thresh,
                        fact_for_cfg, fact_encoding, fact_hash_algo,
                        line_sim,
                        dump_delta=args.dump_delta,
                        fact_for_delta=args.fact_for_delta,
                        keep_going=args.keep_going,
                        use_sim=args.use_sim,
                        sim_thresh=args.sim_thresh,
        )
        if args.max_ntasks:
            pool.max_ntasks = args.max_ntasks
        pool.fill()

    elif args.command == 'collect':
        if load_fact:
            fact_dir = make_factbase_dir(args.basedir, proj_id)
            logger.info('fact dir: "{}"'.format(fact_dir))
            pool = TaskPool(args.basedir, working_dir, conf, False, 
                            load_fact, fact_dir, ignore_unmodified, restrict_fact, 
                            fact_for_changes, fact_for_mapping, fact_for_ast, 
                            fact_into_virtuoso, fact_into_directory, fact_size_thresh,
                            fact_for_cfg, fact_encoding, fact_hash_algo,
                            line_sim)
            pool.load(args.temp_file_size)

    else:
        logger.info('invalid command')
        argparser.print_help()

