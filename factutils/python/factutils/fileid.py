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

import os
import hashlib

from .exn import Invalid_argument
from .const import SUB_SEP, SUB_SUB_SEP

import pathsetup
import dp

class FidEnc:
    FD  = 'FD'
    PVF = 'PVF'

class LocEnc:
    O   = 'O'
    L   = 'L'
    LC  = 'LC'
    LCO = 'LCO'

class Enc:
    FDO    = FidEnc.FD + LocEnc.O
    FDL    = FidEnc.FD + LocEnc.L
    FDLC   = FidEnc.FD + LocEnc.LC
    FDLCO  = FidEnc.FD + LocEnc.LCO
    PVFO   = FidEnc.PVF + LocEnc.O
    PVFL   = FidEnc.PVF + LocEnc.L
    PVFLC  = FidEnc.PVF + LocEnc.LC
    PVFLCO = FidEnc.PVF + LocEnc.LCO

class VerKind:
    REL = 'REL'
    SVN = 'SVNREV'
    GIT = 'GITREV'
    VAR = 'VARIANT'

enum_vkind = [VerKind.REL, VerKind.SVN, VerKind.GIT, VerKind.VAR]

class HashAlgo:
    MD5       = 'MD5'
    SHA1      = 'SHA1'
    SHA224    = 'SHA224'
    SHA256    = 'SHA256'
    SHA384    = 'SHA384'
    SHA512    = 'SHA512'
    RIPEMD160 = 'RIPEMD160'
    GIT       = 'GIT'
    PATH      = 'PATH'

hash_algo_tbl = {
    HashAlgo.MD5       : hashlib.md5,
    HashAlgo.SHA1      : hashlib.sha1,
    HashAlgo.SHA224    : hashlib.sha224,
    HashAlgo.SHA256    : hashlib.sha256,
    HashAlgo.SHA384    : hashlib.sha384,
    HashAlgo.SHA512    : hashlib.sha512,
    HashAlgo.RIPEMD160 : hashlib.new('ripemd160'),
    HashAlgo.GIT       : hashlib.sha1,
    HashAlgo.PATH      : hashlib.sha1,
}



def _compute_hash(algo, f, path=None):
    digest = None
    h = hash_algo_tbl.get(algo, None)
    if h == None:
        raise Invalid_argument(algo)

    content = f.read()

    if algo == HashAlgo.GIT:
        head = 'blob %d\0' % (len(content))
        content = head + content

    elif algo == HashAlgo.PATH:
        if path:
            head = '%s\0' % path
            content = head + content
        else:
            dp.warning('path not specified')

    digest = h(content).hexdigest()

    return digest

def compute_hash(algo, fname):
    digest = None
    f = None
    try:
        f = open(fname, 'r')
        digest = _compute_hash(algo, f, path=fname)

    except Exception as e:
#        dp.warning(str(e))
        raise

    finally:
        if f:
            f.close()

    return digest



def encode_string(s):
    encoded = ''
    for c in s:
        encoded += '%02x' % ord(c)
    return encoded

def decode_string(encoded):
    s = ''
    sz = len(encoded)
    if sz % 2 != 0:
        raise Invalid_argument
    for n in xrange(0, sz, 2):
        try:
            h = encoded[n:n+2]
            s += chr(int(h, 16))
        except:
            raise Invalid_argument

    return s


class FileId(dp.base):
    def __init__(self):
        self._valid = False
        self._encoded = None
        self._enc = None

    def __str__(self):
        return self._encoded

    def __hash__(self):
        return hash(self._encoded)

    def is_valid(self):
        return self._valid

    def encode(self):
        return self._encoded

    def get_enc(self):
        return self._enc


class FileDigest(FileId):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        algo = compos[0]
        digest = compos[1]
        fid = FileDigest(algo, digest=digest)
        return fid

    def __init__(self, algo, path=None, stream=None, digest=None):
        FileId.__init__(self)
        self._algo = algo
        self._enc = FidEnc.FD
        self._digest = digest
        try:
            if path:
                self._digest = compute_hash(algo, path)
                self._valid = True
            elif stream:
                self._digest = _compute_hash(algo, stream)
                self._valid = True
            elif digest:
                self._valid = True
            else:
                self._valid = False
        except Exception as e:
            self.warning(str(e))
            self._valid = False

        if self._valid:
            self._encoded = self._algo + SUB_SEP + self._digest

    def __eq__(self, other):
        res = False
        if isinstance(other, FileDigest):
            res = self._algo == other._algo and self._digest == other._digest
        return res

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_algorithm(self):
        return self._algo

    def get_value(self):
        return self._digest


class Version(dp.base):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SUB_SEP)
        kind = compos[0]
        encoded_ver = compos[1]
        ver = decode_string(encoded_ver)
        obj = Version(kind, ver)
        return obj

    def __init__(self, k, v):
        self._valid = k in enum_vkind
        self._kind = k
        self._ver = v
        self._encoded = None
        if self._valid:
            self._encoded = self._kind + SUB_SUB_SEP + encode_string(self._ver)

    def __eq__(self, other):
        res = False
        if isinstance(other, Version):
            res = self._kind == other._kind and self._ver == other._ver

        return res

    def __ne__(self, other):
        return not self.__eq__(other)

    def is_valid(self):
        return self._valid

    def get_kind(self):
        return self._kind

    def get_name(self):
        return self._ver

    def encode(self):
        return self._encoded


class ProjRelPath(dp.base):
    @classmethod
    def from_encoded(cls, encoded): # should be fixed
        proj_rel_path = decode_string(encoded)
        obj = ProjRelPath('', proj_rel_path, proj_rel_path=proj_rel_path)
        return obj

    def __init__(self, proj_root, path, proj_rel_path=None):
        self._proj_root = os.path.abspath(proj_root)
        self._path = os.path.abspath(path)
        if proj_rel_path:
            self._proj_rel_path = proj_rel_path
        else:
            if self._path and self._proj_root:
                self._proj_rel_path = os.path.relpath(self._path, self._proj_root)
            else:
                self._proj_rel_path = ''
        self._encoded = None

    def __eq__(self, other):
        res = False
        if isinstance(other, ProjRelPath):
            res = self._proj_rel_path == other._proj_rel_path

        return res

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_proj_root(self):
        return self._proj_root

    def get_path(self):
        return self._path

    def get_proj_rel_path(self):
        return self._proj_rel_path

    def encode(self):
        if self._encoded == None:
            self._encoded = encode_string(self._proj_rel_path)
        return self._encoded


class FileDesc(FileId):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        proj = decode_string(compos[0])
        ver = Version.from_encoded(compos[1])
        if len(compos) > 2:
            rel_path = ProjRelPath.from_encoded(compos[2])
        else:
            rel_path = None
        fid = FileDesc(proj, ver, rel_path)
        return fid

    def __init__(self, proj, ver, rel_path):
        FileId.__init__(self)
        self._valid = reduce(lambda x,y: x and y, [isinstance(ver, Version), 
                                                   ver.is_valid(), 
                                                   isinstance(rel_path, ProjRelPath) or rel_path == None])
        self._proj = proj
        self._ver = ver
        self._proj_rel_path = rel_path
        self._enc = FidEnc.PVF
        self._encoded = None
        if self._valid:
            if self._proj_rel_path:
                self._encoded = SUB_SEP.join([encode_string(self._proj), 
                                              self._ver.encode(),
                                              self._proj_rel_path.encode()])

            else:
                self._encoded = SUB_SEP.join([encode_string(self._proj), 
                                              self._ver.encode()])
                


    def __eq__(self, other):
        res = False
        if isinstance(other, FileDesc):
            res = reduce(lambda x,y: x and y, [self._proj == other._proj,
                                               self._ver == other._ver,
                                               self._proj_rel_path == other._proj_rel_path])
        return res

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_project(self):
        return self._proj

    def get_version(self):
        return self._ver

    def get_proj_rel_path(self):
        return self._proj_rel_path

    def encode(self):
        return self._encoded

    def get_enc(self):
        return self._enc

    def get_value(self):
        return self.encode()



def from_encoded(encoded):
    fid = None
    is_FD = False
    for algo in hash_algo_tbl.keys():
        if encoded.startswith(algo):
            is_FD = True
            break

    if is_FD:
        fid = FileDigest.from_encoded(encoded)
    else:
        fid = FileDesc.from_encoded(encoded)

    return fid
