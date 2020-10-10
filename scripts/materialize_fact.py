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

import pathsetup
import dp
import project
import sparql
from ns import VER_NS, FB_NS
import virtuoso
from virtuoso import VIRTUOSO_PW, DEFAULT_PORT

MAX_VER_TRIPLES = 128

VER_ORDER_QUERY = '''
PREFIX ver: <%(ver_ns)s>
WITH <%(graph)s>
INSERT {
%(ver_next_triples)s
}
'''

INSERT_PAT = re.compile(r'insert', re.I)

class Materializer(dp.base):
    def __init__(self, qdir, queries, proj_id,
                 method='odbc', pw=VIRTUOSO_PW, port=DEFAULT_PORT):
        self._query_dir = qdir
        self._queries = queries
        self._proj_id = proj_id
        self._graph_uri = FB_NS + proj_id
        self._sparql = sparql.get_driver(method, pw=pw, port=port)
        self._port = port
        self._pw = pw
        try:
            self._conf = project.get_conf(proj_id)
        except:
            self._conf = None


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
            self.debug('query:\n%s' % q)
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
            self.error(str(e))
        return query


    def materialize(self):
        self.message('materializing for "%s"...' % self._proj_id)

        self.message('materializing version order...')
        self.insert_ver_next_triples()

        for lang in self._queries.keys():

            for qname in self._queries[lang]:
                self.message('processing \"%s\" for %s...' % (qname, lang)),
                sys.stdout.flush()

                query = self.get_query(lang, qname)
                self._sparql.execute(query)

                self.message('done.')

        virt = virtuoso.base(pw=self._pw, port=self._port)
        rc = virt.checkpoint()

        return rc



def main(qdir, queries, desc, pw=VIRTUOSO_PW):
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('proj_id', type=str, help='project id')
    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')
    parser.add_argument('-p', '--port', dest='port', default=1111,
                        metavar='PORT', type=int, help='port number')


    args = parser.parse_args()

    dp.debug_flag = args.debug

    m = Materializer(qdir, queries, args.proj_id, pw=pw, port=args.port)

    m.materialize()


if __name__ == '__main__':
    main(None, {}, 'materialize version fact')

