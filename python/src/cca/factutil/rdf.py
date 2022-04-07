#!/usr/bin/env python3

'''
  Factutil: helper scripts for source code entities

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
from functools import reduce

import rdflib
from rdflib.namespace import XSD

import logging

logger = logging.getLogger()


def uri_split(uri):
    lname = uri.split('/')[-1].split('#')[-1]
    ns = uri[:len(uri)-len(lname)]
    return ns, lname


class RDFNode(object):
    def __init__(self, nd):
        self._valid = True
        self._node = nd

    def __eq__(self, other):
        res = False
        if isinstance(other, RDFNode):
            res = self._node == other._node

        return res

    def is_valid(self):
        return self._valid

    def as_node(self):
        return self._node


class Resource(RDFNode):
    def __init__(self, uri=None, **args):
        nd = args.get('node', None)
        if nd is not None:
            RDFNode.__init__(self, nd)
        else:
            if uri is not None:
                try:
                    RDFNode.__init__(self, rdflib.term.URIRef(uri))
                except Exception:
                    logger.warning('uri="%s"(%s)' % (uri, str(type(uri))))
                    raise
            else:
                RDFNode.__init__(self, rdflib.term.BNode())  # blank node

    def __eq__(self, other):
        res = False
        if isinstance(other, Resource):
            if isinstance(self._node, rdflib.term.URIRef) \
               and isinstance(other._node, rdflib.term.URIRef):
                res = self.get_uri() == other.get_uri()
            else:
                res = self._node == other._node

        return res

    def __lt__(self, other):
        return str(self.get_uri()) < str(other.get_uri())

    def __gt__(self, other):
        return str(self.get_uri()) > str(other.get_uri())

    def __le__(self, other):
        self.__eq__(other) or self.__lt__(other)

    def __ge__(self, other):
        self.__eq__(other) or self.__gt__(other)

    def __hash__(self):
        return str(self.get_uri()).__hash__()

    def __str__(self):
        return '<%s>' % self.get_uri()

    def get_uri(self):
        return str(str(self.as_node()))

    def get_namespane(self):
        ns, ln = uri_split(self.get_uri())
        return ns

    def get_local_name(self):
        ns, ln = uri_split(self.get_uri())
        return ln


class Literal(RDFNode):
    def __init__(self, literal="", **args):
        nd = args.get('node', None)
        if nd is not None:
            RDFNode.__init__(self, nd)
        else:
            RDFNode.__init__(self, rdflib.Literal(literal, **args))

    def __eq__(self, other):
        res = False
        if isinstance(other, Literal):
            res = self._node.eq(other._node)
        return res

    def __str__(self):
        return '"%s"' % self.get_content()

    def get_content(self):
        return self._node.value


def make_literal(x):
    lit = None
    if isinstance(x, bool):
        lit = Literal(literal=str(x).lower(), datatype=XSD.boolean)
    elif isinstance(x, int):
        if x >= 0:
            lit = Literal(literal=str(x), datatype=XSD.nonNegativeInteger)
        else:
            lit = Literal(literal=str(x), datatype=XSD.integer)
    elif isinstance(x, float):
        lit = Literal(literal=str(x), datatype=XSD.double)
    # elif isinstance(x, str):
    #     lit = Literal(literal=x.encode('utf-8'))
    else:
        lit = Literal(literal=str(x))

    return lit


class Predicate(Resource):
    def __init__(self, ns=None, lname=None, **args):
        self._lname = None
        self._ns = None

        uri = None
        node = args.get('node', None)

        if ns is None or lname is None:
            uri = args.get('uri', None)

            if uri is None:
                if node is not None:
                    if isinstance(node, rdflib.term.URIRef):
                        uri = str(str(node))
            if uri is not None:
                self._ns, self._lname = uri_split(uri)

        else:
            self._ns = ns
            self._lname = lname
            uri = ns + lname

        Resource.__init__(self, uri, **args)

    def __str__(self):
        return '<%s>' % self.get_uri()

    def get_namespace(self):
        return self._ns

    def get_local_name(self):
        return self._lname


class Statement(object):
    def __init__(self, subject=None, predicate=None, object=None, **args):
        try:
            stmt = args['statement']
            self.subject = stmt.subject
            self.predicate = stmt.predicate
            self.object = stmt.object
            self._stmt = stmt._stmt

        except KeyError:
            self.subject = subject
            self.predicate = predicate
            self.object = object
            s = None
            p = None
            o = None
            if isinstance(subject, Resource):
                s = subject.as_node()
            if isinstance(predicate, Predicate):
                p = predicate.as_node()
            if isinstance(object, RDFNode):
                o = object.as_node()

            self._stmt = (s, p, o)

    def __eq__(self, other):
        res = False
        if isinstance(other, Statement):
            res = reduce(lambda x, y: x and y,
                         [self.subject == other.subject,
                          self.predicate == other.predicate,
                          self.object == other.object])
        return res


class Graph(object):
    def __init__(self, ns_tbl, large=False):

        if large:
            self._model = rdflib.graph.Graph('BerkeleyDB')
        else:
            self._model = rdflib.graph.Graph('Memory')

        self._g_pred_map = {}
        self._pred_tbl = {}

        self.l_true = Literal('true')
        self.l_false = Literal('false')

        self.namespace_tbl = ns_tbl

    def set_namespace(self, prefix, uri):
        self.namespace_tbl[prefix] = uri

    def contains(self, s, p, o):
        stmt = self._create_statement(s, p, o)
        return (stmt in self._model)

    def find_statements(self, t):
        return self._model.triples(t)

    def size(self):
        return len(self._model)

    def _add(self, subj, pred, obj):
        self._model.add((subj.as_node(), pred.as_node(), obj.as_node()))

    def _create_statement(self, subj, pred, obj):
        s = None
        p = None
        o = None
        if subj:
            s = subj.as_node()
        if pred:
            p = pred.as_node()
        if obj:
            o = obj.as_node()
        return (s, p, o)

    def _guess_fmt(self, path):
        fmt = ''
        if path.endswith('.nt'):
            fmt = 'nt'
        elif path.endswith('.ttl'):
            fmt = 'turtle'
        elif path.endswith('.rdf'):
            fmt = 'xml'
        if path.endswith('.nt.gz'):
            fmt = 'nt'
        elif path.endswith('.ttl.gz'):
            fmt = 'turtle'
        elif path.endswith('.rdf.gz'):
            fmt = 'xml'
        return fmt

    def _mktemp(self):
        (fd, path) = tempfile.mkstemp()
        os.close(fd)
        return path

    def _gzipped(self, path):
        return path.endswith('.gz')

    def _gzip(self, from_file, to_file):
        f_from = open(from_file, 'rb')
        f_to = gzip.open(to_file, 'wb')
        f_to.writelines(f_from)
        f_to.close()
        f_from.close()

    def _gunzip(self, from_file, to_file):
        f_from = gzip.open(from_file, 'rb')
        f_to = open(to_file, 'wb')
        f_to.writelines(f_from)
        f_to.close()
        f_from.close()

    def write(self, path, fmt='', base_uri=None):
        if fmt == '':
            fmt = self._guess_fmt(path)

        gzipped_path = None

        if self._gzipped(path):
            gzipped_path = path
            tmp = self._mktemp()
            path = tmp

        for (prefix, uri) in self.namespace_tbl.items():
            self._model.bind(prefix, uri)

        logger.info('writing to "%s"...' % path)
        d = os.path.dirname(path)
        if d != '' and not os.path.exists(d):
            logger.warning('No such directory: "%s"' % d)
            logger.info('creating "%s"...' % d)
            os.makedirs(d)
        self._model.serialize(path, format=fmt, base=base_uri)
        logger.info('done.')

        if gzipped_path:
            self._gzip(path, gzipped_path)
            os.unlink(path)

    def read(self, path, fmt='', base_uri=None):
        if fmt == '':
            fmt = self._guess_fmt(path)

        gzipped = False

        if self._gzipped(path):
            gzipped = True
            tmp = self._mktemp()
            self._gunzip(path, tmp)
            path = tmp

        logger.info('reading "%s"...' % path)
        self._model.parse(location=path, format=fmt, base=base_uri)
        logger.info('done.')

        if gzipped:
            os.unlink(tmp)

    def query(self, qstr, base_uri=None):
        results = self._model.query(qstr, initNs=self.namespace_tbl)
        return results


if __name__ == '__main__':
    pass
