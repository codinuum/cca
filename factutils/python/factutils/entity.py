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

from .const import ENTITY_NS, EXTERNAL_NS, SEP
from .exn import Invalid_argument
from .rdf import Resource
from .range import LCRange, ORange, LORange, LCORange, MAX_RANGE
from .fileid import FileDigest, FileDesc
from . import fileid
from . import range

import pathsetup
import dp



class External(Resource):
    def __init__(self, sym=None, **args):
        nd = args.get('node', None)
        if nd != None:
            Resource.__init__(self, node=nd)
        else:
            if sym != None:
                Resource.__init__(self, EXTERNAL_NS + sym)
            else:
                Resource.__init__(self, EXTERNAL_NS + '???')
                self._valid = False
        
    def __str__(self):
        return '<%s>' % self.get_uri()

class SourceCodeEntity(Resource):
    def __init__(self, **args):
        Resource.__init__(self)
        self._enc = None
        self._file_id = None
        self._range = MAX_RANGE
        self._local_name = None
        self._node = None
        self._valid = False

        try:
            self._file_id = FileDesc(args['project'],
                                     args['version'],
                                     args['proj_rel_path'])
        except KeyError:
            pass

        try:
            self._file_id = args['file_id']
        except KeyError:
            pass

        try:
            self._range = args['range']
        except KeyError:
            pass

        try:
            sl = args['start_line']
            sc = args['start_column']
            so = args['start_offset']
            el = args['end_line']
            ec = args['end_column']
            eo = args['end_offset']
            self._range = LCORange(sl, sc, so, el, ec, eo)
        except KeyError:
            pass

        try:
            sl = args['start_line']
            sc = args['start_column']
            el = args['end_line']
            ec = args['end_column']
            self._range = LCRange(sl, sc, el, ec)
        except KeyError:
            pass

        try:
            sl = args['start_line']
            so = args['start_offset']
            el = args['end_line']
            eo = args['end_offset']
            self._range = LORange(sl, so, el, eo)
        except KeyError:
            pass

        try:
            so = args['start_offset']
            eo = args['end_offset']
            self._range = ORange(so, eo)
        except KeyError:
            pass

        try:
            self._node = RDF.Node(uri_string=args['uri'])
        except KeyError:
            pass


        if self._file_id and self._range:
            self._enc = self._file_id.get_enc()

            self._valid = self._file_id.is_valid() & self._range.is_valid()

            if self._range == MAX_RANGE:
                compos = [self._enc, self._file_id.encode()]
            else:
                self._enc += self._range.get_enc()
                compos = [self._enc, self._file_id.encode(), self._range.encode()]

            self._local_name = SEP.join(compos)
            uri = RDF.Uri(ENTITY_NS + self._local_name)
            self._node = RDF.Node(uri)

        elif self._node:
            uri_str = str(self._node.uri)
            if uri_str.startswith(ENTITY_NS):
                self._local_name = uri_str.replace(ENTITY_NS, '')

                try:
                    compos = self._local_name.split(SEP)
                    self._enc = compos[0]
                    self._file_id = fileid.from_encoded(compos[1])
                    try:
                        self._range = range.from_encoded(compos[2])
                    except IndexError:
                        pass

                    self._valid = self._file_id.is_valid() & self._range.is_valid()

                except Exception as e:
                    #self.warning(str(e))
                    self._valid = False
                
            else:
                self._valid = False
            
        else:
            self._valid = False



    def __str__(self):
        return '<%s>' % self.get_uri()


    def get_encoding(self):
        return self._enc

    def get_range(self):
        return self._range

    def get_file_id(self):
        return self._file_id

    def get_uri(self):
        return self._node.uri

    def get_local_name(self):
        return self._local_name

    def contains(self, other):
        b = False
        if self._file_id == other._file_id:
            b = self._range.contains(other._range)
#            self.debug('%s vs %s --> %s' % (self.get_range(), other.get_range(), b))
        
        return b

    def is_file(self):
        return self._range == MAX_RANGE
