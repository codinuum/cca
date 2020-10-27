#!/usr/bin/env python3


'''
  An ontology loader

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

import virtuoso

GRAPH_URI = 'http://codinuum.com/ont/cpi'
RULE_NAME = 'ont.cpi'

FACT_DIR = os.path.join(virtuoso.VTMP_DIR, 'ontologies')

def load(db_dir, fact_dir,
         graph_uri=GRAPH_URI,
         rule_name=RULE_NAME,
         port=virtuoso.DEFAULT_PORT,
         pw=virtuoso.VIRTUOSO_PW):

    rc = 0
    loader = virtuoso.Loader(db_dir, port=port, pw=pw)
    rc = loader.load(graph_uri, fact_dir, ['.rdf'])
    if rc == 0:
        rc = loader.rdfs_rule_set(rule_name, graph_uri)
        if rc == 0:
            rc = loader.checkpoint()
    return rc

if __name__ == '__main__':
    load(virtuoso.DB_DIR, FACT_DIR)
