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

import RDF

import pathsetup
import dp


def mkuri(s):
    return RDF.Uri(s)

def uri_split(uri):
    lname = uri.split('/')[-1].split('#')[-1]
    ns = uri[:len(uri)-len(lname)]
    return ns, lname


class RDFNode(dp.base):
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
        if nd != None:
            RDFNode.__init__(self, nd)
        else:
            if uri != None:
                if isinstance(uri, str):
                    uri = uri.encode()
                try:
                    RDFNode.__init__(self, RDF.Node(uri_string=uri))
                except:
                    self.warning('uri="%s"(%s)' % (uri, str(type(uri))))
                    raise
            else:
                RDFNode.__init__(self, RDF.Node()) # blank node

    def __eq__(self, other):
        res = False
        if isinstance(other, Resource):
            if self._node.is_resource() and other._node.is_resource():
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
        return str(self.as_node().uri)

    def get_namespane(self):
        ns, ln = uri_split(self.get_uri())
        return ns

    def get_local_name(self):
        ns, ln = uri_split(self.get_uri())
        return ln


class Literal(RDFNode):
    def __init__(self, literal="", **args):
        nd = args.get('node', None)
        if nd != None:
            RDFNode.__init__(self, nd)
        else:
            RDFNode.__init__(self, RDF.Node(literal=literal, **args))

    def __eq__(self, other):
        res = False
        if isinstance(other, Literal):
            res = self.get_content() == other.get_content()

        return res

    def __str__(self):
        return '"%s"' % self.get_content()

    def get_content(self):
        return self._node.literal_value



class Predicate(Resource):
    def __init__(self, ns=None, lname=None, **args):
        self._lname = None
        self._ns = None

        uri = None
        node = args.get('node', None)

        if ns == None or lname==None:
            uri = args.get('uri', None)

            if uri == None:
                if node != None:
                    uri = str(node.uri)
            
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
