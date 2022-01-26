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

import logging

from .const import RELEASE_NS, SVNREV_NS, GITREV_NS, GUARD_NS
from .rdf import Graph, Resource, Predicate


logger = logging.getLogger()


class Fact(Graph):

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
        for stmt in self.find_statements(q):
            g = Resource(node=stmt.subject)
            if self.contains(g, gp, o):
                guards.append(g)
        return guards

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

            if blk is None:
                blk = Resource()

            self._add(blk, attr, value)

    def addStatement(self, stmt, attr=None, value=None):
        self.add(stmt.subject, stmt.predicate, stmt.object, attr, value)
