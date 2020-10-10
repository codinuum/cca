#!/usr/bin/env python3

'''
  Factutils: helper scripts for source code entities

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
import tempfile
import gzip
import RDF

from const import SPEC_NS, PREDICATE_NS, RELEASE_NS, SVNREV_NS, GITREV_NS, GUARD_NS
from rdf import Resource, Literal, Predicate, RDFNode

import pathsetup
import dp

class Statement(dp.base):
    def __init__(self, subject=None, predicate=None, object=None, **args):
        try:
            stmt = args['statement']
            self.subject = stmt.subject
            self.predicate = stmt.predicate
            self.object = stmt.object
            self._stmt = RDF.Statement(statement=stmt._stmt)

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

            self._stmt = RDF.Statement(s, p, o)


    def __eq__(self, other):
        res = False
        if isinstance(other, Statement):
            res = reduce(lambda x,y: x and y, [self.subject == other.subject,
                                               self.predicate == other.predicate,
                                               self.object == other.object])
        return res



class Fact(dp.base):
    def __init__(self, ns_tbl, large=False):

        if large:
            self._storage = RDF.HashStorage('db4', options="new='yes',hash-type='bdb'")
        else:
            self._storage = RDF.MemoryStorage()

        self._model = RDF.Model(self._storage)

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

    def create_release_version(self, rel):
        return Resource(RELEASE_NS + rel)

    def create_svn_revision(self, rev):
        s = '%s%s' % (SVNREV_NS, rev)
        return Resource(s)

    def create_git_revision(self, rev):
        s = '%s%s' % (GITREV_NS, rev)
        return Resource(s)

    def get_guard_pred(self, pred):
        g_pred = None
        try:
            g_pred = self._g_pred_map[pred]

        except KeyError:
            g_pred = Predicate(GUARD_NS+'?orig='+pred.get_namespace(), 
                               pred.get_local_name())
            self._g_pred_map[pred] = g_pred

        return g_pred

    def list_guards(self, s, p, o):
        guards = []
        gp = self.get_guard_pred(p)
        q = self._create_statement(None, gp, s)
        for stmt in self._model.find_statements(q):
            g = Resource(node=stmt.subject)
            if self.contains(g, gp, o):
                guards.append(g)

        return guards

    def size(self):
        return self._model.size()

    def _add(self, subj, pred, obj):
        self._model.add(subj.as_node(), pred.as_node(), obj.as_node())

    def add(self, subj, pred, obj, attr=None, value=None):
        self._add(subj, pred, obj)
        if attr and value:
            blk = None
            guards = self.list_guards(subj, pred, obj)

            if len(guards) == 0:
                blk = Resource()
                g_pred = self.get_guard_pred(pred)
                self._add(blk, g_pred, subj)
                self._add(blk, g_pred, obj)

            else:
                blk = guards[0]

            if blk == None:
                blk = Resource()

            self._add(blk, attr, value)


    def addStatement(self, stmt, attr=None, value=None):
        self.add(stmt.subject, stmt.predicate, stmt.object, attr, value)

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
        return RDF.Statement(s, p, o)

    def _guess_fmt(self, path):
        fmt = ''
        if path.endswith('.nt'):
            fmt = 'ntriples'
        elif path.endswith('.ttl'):
            fmt = 'turtle'
        elif path.endswith('.rdf'):
            fmt = 'rdfxml'
        if path.endswith('.nt.gz'):
            fmt = 'ntriples'
        elif path.endswith('.ttl.gz'):
            fmt = 'turtle'
        elif path.endswith('.rdf.gz'):
            fmt = 'rdfxml'

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

        serializer = RDF.Serializer(name=fmt)
        for (prefix, uri) in self.namespace_tbl.iteritems():
            serializer.set_namespace(prefix, uri)

        self.message('writing to "%s"...' % path)
        d = os.path.dirname(path)
        if d != '' and not os.path.exists(d):
            self.warning('No such directory: "%s"' % d)
            self.message('creating "%s"...' % d)
            os.makedirs(d)
        serializer.serialize_model_to_file(path, self._model, base_uri=base_uri)
        self.message('done.')

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

        parser = RDF.Parser(name=fmt)
        self.message('reading "%s"...' % path)
        parser.parse_into_model(self._model,
                                'file://' + os.path.abspath(path),
                                base_uri=base_uri)
        self.message('done.')

        if gzipped:
            os.unlink(tmp)

    
    def query(self, qstr, base_uri=None):
        q = RDF.SPARQLQuery(qstr, base_uri=base_uri)
        results = q.execute(self._model)
        return results
