#!/usr/bin/env python3


'''
  A SPARQL driver

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

# Fortran namespaces added by Masatomo Hashimoto <m.hashimoto@riken.jp>

import pathsetup
import dp
from siteconf import SPARQL_ENDPOINT
from virtuoso import ODBCDriver, VIRTUOSO_PW, VIRTUOSO_PORT, get_odbc_connect_string
import ns
from factutils.const import ENTITY_NS, VARIANT_NS, SVNREV_NS, GITREV_NS, RELEASE_NS



NAMESPACES = { 'xsd'  : ns.XSD_NS,
               'owl'  : ns.OWL_NS,
               'rdf'  : ns.RDF_NS,
               'fb'   : ns.FB_NS,
               'src'  : ns.SRC_NS,
               'ver'  : ns.VER_NS,

               'git'  : ns.GIT_NS,

               'ent'      : ENTITY_NS,
               'variant'  : VARIANT_NS,
               'svnrev'   : SVNREV_NS,
               'gitrev'   : GITREV_NS,
               'rel'      : RELEASE_NS,

               'f'    : ns.F_NS,
               'pa'   : ns.PA_NS,
               'fjpa' : ns.FJPA_NS,
               'fpt'  : ns.FPT_NS,

               'fjpadata' : ns.PREFIX_TBL['fjpadata'],
           }



def get_localname(s):
    res = s
    if s:
        try:
            if s.startswith('http://'):
                res = (s.split('/'))[-1].split('#')[-1]
        except Exception as e:
            dp.warning(str(e))

    return res



class Driver(dp.base):
    def __init__(self):
        self._ns_tbl = {}
        for (n, p) in NAMESPACES.items():
            self._ns_tbl[p] = n

    def to_prefixed_form(self, v):
        r = v
        if v:
            try:
                for p in self._ns_tbl.keys():
                    if v.startswith(p):
                        r = '%s:%s' % (self._ns_tbl[p], v[len(p):])
                        break
            except Exception as e:
                dp.warning('"%s": %s' % (v, e))

        return r


    def execute(self, q):
        pass

    def query(self, q, abbrev=False):
        return None

    def fetchone(self, q, abbrev=False):
        return None


class VirtuosoODBCDriver(ODBCDriver, Driver):
    def __init__(self, pw=VIRTUOSO_PW, port=VIRTUOSO_PORT):
        connect_string = get_odbc_connect_string(pwd=pw, port=port)
        ODBCDriver.__init__(self, connect_string)
        Driver.__init__(self)

    def conv_row(self, row, abbrev=False):
        if row and abbrev:
            for (k, v) in row.items():
                row[k] = self.to_prefixed_form(v)

        return row

    def query(self, q, abbrev=False):
        for qvs, row in ODBCDriver.query(self, 'SPARQL\n'+q):
            yield qvs, self.conv_row(row, abbrev)

    def execute(self, q):
        ODBCDriver.execute(self, 'SPARQL\n'+q)

    def fetchone(self, q, abbrev=False):
        r = ODBCDriver.fetchone(self, 'SPARQL\n'+q)
        if r:
            r = self.conv_row(r, abbrev)
        return r



class VirtuosoHTTPDriver(Driver):
    def __init__(self, endpoint=SPARQL_ENDPOINT):
        self._endpoint = endpoint

    def conv_binding(self, b, abbrev=False):
        d = {}
        for k in b.keys():
            data = b[k]
            v = str(data['value'])
            ty = data['type']
            if ty == 'typed-literal':
                dty = self.to_prefixed_form(data['datatype'])
                self.debug('%s (%s)' % (v, dty))
                if dty == 'xsd:decimal':
                    v = float(v)
                elif dty == 'xsd:integer':
                    v = int(v)

            if abbrev:
                if ty == 'uri':
                    v = self.to_prefixed_form(v)

            d[k] = v
        return d

    def _exec(self, q, limit=-1):
        import json
        from urllib.parse import urlencode
        from urllib.request import Request, urlopen

        format = 'application/json'

        if limit < 0:
            maxrows = ''
        else:
            maxrows = str(limit)

        params = { 
            'query'  : q,
            'format' : format,
            'maxrows' : maxrows,
        }

        qpart = urlencode(params)

        req = Request(self._endpoint, qpart)

        response = urlopen(req).read()
        
        result = json.loads(response)

        return result

    def execute(self, q):
        self._exec(q)

    def fetchone(self, q, abbrev=False):
        row = None
        try:
            r = self._exec(q, limit=1)
            b = r['results']['bindings'][0]
            row = self.conv_binding(b, abbrev)
        except:
            pass

        return row

    def query(self, q, abbrev=False, limit=-1):
        result = self._exec(q, limit)
        for b in result['results']['bindings']:
            qvs = [str(v) for v in result['head']['vars']]
            yield qvs, self.conv_binding(b, abbrev)




def get_driver(method='http', pw=VIRTUOSO_PW, port=VIRTUOSO_PORT):
    driver = None
    if method == 'http':
        driver = VirtuosoHTTPDriver()
    elif method == 'odbc':
        driver = VirtuosoODBCDriver(pw=pw, port=port)
    else:
        dp.error('unknown method: "%s"' % method)
    return driver


def query():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='Execute SPARQL Query',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('query_file', type=str, help='query file')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true', help='enable debug printing')

    parser.add_argument('-m', '--method', dest='method', default='odbc',
                        metavar='METHOD', type=str, help='execute query via METHOD (http|odbc)')


    args = parser.parse_args()

    dp.debug_flag = args.debug

    qfile = args.query_file

    dp.message('method: "%s"' % args.method)
    dp.message('query:  "%s"' % qfile)


    driver = get_driver(args.method)

    count = 0

    try:
        f = open(qfile, 'r')
        q = f.read()
        f.close()

        for vs, r in driver.query(q, abbrev=True):
            row = []
            for v in vs:
                row.append('  %s="%s"' % (v, r[v]))
            print('* row[%d]' % count)
            print('\n'.join(row))
            count += 1

    except Exception as e:
        #dp.error(str(e))
        raise

    print('%d rows' % count)


def test():
    #sparql = VirtuosoODBCDriver()
    sparql = VirtuosoHTTPDriver()

    q = 'DEFINE input:inference "ont.cpi" SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10'

    for r in sparql.query(q):
        print(r)


if __name__ == '__main__':
    query()

