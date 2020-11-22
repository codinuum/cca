#!/usr/bin/env python3


'''
  Base library for fact materialization

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

import os.path
import sys
import re
import logging

import pathsetup
import project
import sparql
from ns import VER_NS, FB_NS
import virtuoso
from virtuoso import VIRTUOSO_PW, VIRTUOSO_PORT
from common import setup_logger

logger = logging.getLogger()


MAX_VER_TRIPLES = 128

VER_ORDER_QUERY = '''
PREFIX ver: <%(ver_ns)s>
WITH <%(graph)s>
INSERT {
%(ver_next_triples)s
}
'''

INSERT_PAT = re.compile(r'insert', re.I)

class Materializer(object):
    def __init__(self, qdir, queries, proj_id,
                 method='odbc', pw=VIRTUOSO_PW, port=VIRTUOSO_PORT, conf=None):
        self._query_dir = qdir
        self._queries = queries
        self._proj_id = proj_id
        self._graph_uri = FB_NS + proj_id
        self._sparql = sparql.get_driver(method, pw=pw, port=port)
        self._port = port
        self._pw = pw
        if conf == None:
            self._conf = project.get_conf(proj_id)
        else:
            self._conf = conf


    def make_ver_next_triples(self):
        triples = []

        ts = []

        if self._conf:
            if self._conf.vpairs:
                for (u1, u2) in self._conf.vURIpairs:
                    if len(ts) >= MAX_VER_TRIPLES:
                        triples.append(ts)
                        ts = []
                    ts.append('<%s> ver:next <%s> .' % (u1, u2))
            else:
                uris = self._conf.versionURIs
                for i in range(self._conf.nversions - 1):
                    if len(ts) >= MAX_VER_TRIPLES:
                        triples.append(ts)
                        ts = []
                    ts.append('<%s> ver:next <%s> .' % (uris[i], uris[i+1]))

        if ts:
            triples.append(ts)

        return triples

    def insert_ver_next_triples(self):
        for triples in self.make_ver_next_triples():

            params = { 'ver_ns' : VER_NS,
                       'graph' : self._graph_uri, 
                       'ver_next_triples' : '\n'.join(triples) }

            q = VER_ORDER_QUERY % params
            logger.debug('query:\n%s' % q)
            self._sparql.execute(q)

    def get_query(self, lang, name):
        query = None
        path = os.path.join(self._query_dir, lang, name)
        try:
            f = open(path, 'r')
            q = f.read()
            query = INSERT_PAT.sub('WITH <%s>\nINSERT' % self._graph_uri, q, count=1).rstrip('\n ;')
            f.close()
        except Exception as e:
            logger.error(str(e))
        return query


    def materialize(self):
        logger.info('materializing for "%s"...' % self._proj_id)

        logger.info('materializing version order...')
        self.insert_ver_next_triples()

        for lang in self._queries.keys():

            for qname in self._queries[lang]:
                logger.info('processing \"%s\" for %s...' % (qname, lang)),
                sys.stdout.flush()

                query = self.get_query(lang, qname)
                self._sparql.execute(query)

                logger.info('done.')

        virt = virtuoso.base(pw=self._pw, port=self._port)
        rc = virt.checkpoint()

        return rc



def main(qdir, queries, desc):
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('proj_id', type=str, help='project id')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('--port', dest='port', default=VIRTUOSO_PORT,
                        metavar='PORT', type=int, help='port number')

    parser.add_argument('--pw', dest='pw', metavar='PASSWORD', default=VIRTUOSO_PW,
                        help='set password to access FB')


    args = parser.parse_args()

    log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)


    m = Materializer(qdir, queries, args.proj_id, pw=args.pw, port=args.port)

    m.materialize()


if __name__ == '__main__':
    main(None, {}, 'materialize version fact')

