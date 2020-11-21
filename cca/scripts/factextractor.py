#!/usr/bin/env python3

'''
  factextractor.py

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

import logging

import pathsetup
import project

from factutils.const import SEP, SUB_SEP
from factutils.fileid import HashAlgo, FidEnc, Enc, FileDigest, FileDesc
from factutils.fileid import compute_hash, Version, ProjRelPath, VerKind
from factutils.rdf import Resource, Predicate, Literal, mkuri
from ns import XSD_NS, RDF_NS

logger = logging.getLogger()

DT_BOOLEAN = mkuri(XSD_NS + 'boolean')
DT_DOUBLE  = mkuri(XSD_NS + 'double')
DT_NN_INT  = mkuri(XSD_NS + 'nonNegativeInteger')
DT_INT     = mkuri(XSD_NS + 'integer')

P_TYPE = Predicate(ns=RDF_NS, lname='type')


def make_literal(x):
    lit = None
    if isinstance(x, bool):
        lit = Literal(literal=str(x).lower(), datatype=DT_BOOLEAN)
    elif isinstance(x, int):
        if x >= 0:
            lit = Literal(literal=str(x), datatype=DT_NN_INT)
        else:
            lit = Literal(literal=str(x), datatype=DT_INT)
    elif isinstance(x, float):
        lit = Literal(literal=str(x), datatype=DT_DOUBLE)
    # elif isinstance(x, str):
    #     lit = Literal(literal=x.encode('utf-8'))
    else:
        lit = Literal(literal=str(x))

    return lit





def make_file_hash_repr(fname, algo=HashAlgo.MD5):
    hv = compute_hash(algo, fname)
    lname = algo + SUB_SEP + hv
    return lname

def compo_join(*compos):
    return SEP.join(compos)


class base(object):

    def __init__(self, proj_id, encoding=Enc.FDLCO, algo=HashAlgo.MD5):

        self._proj_id = proj_id

        logger.info('encoding="{}" algo="{}"'.format(encoding, algo))
        self._encoding = encoding
        self._algo = algo

        self.get_fid = None
        if self._encoding.startswith(FidEnc.FD):
            self.get_fid = self.get_file_digest
        elif self._encoding.startswith(FidEnc.PVF):
            self.get_fid = self.get_file_desc
        else:
            logger.error('invalid encoding: {}'.format(self._encoding))

    def get_file_digest(self, root_path, ver, path):
        fd = FileDigest(self._algo, path)
        return fd

    def get_file_desc(self, root_path, ver, path):
        v = Version(VerKind.REL, ver) #
        prp = ProjRelPath(root_path, path)
        fd = FileDesc(self._proj_id, v, prp)
        return fd

