#!/usr/bin/env python3


'''
  Configuration for CCA

  Copyright 2012-2017 Codinuum Software Lab <http://codinuum.com>

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
import re

import pathsetup
import dp
import Git2
from factutils.fileid import HashAlgo, VerKind
import ns
from Git2 import shorten_sha
from siteconf import PROJECTS_DIR

REL_INST_NS     = ns.NS_TBL['rel_ns']
SVNREV_INST_NS  = ns.NS_TBL['svnrev_ns']
GITREV_INST_NS  = ns.NS_TBL['gitrev_ns']
VARIANT_INST_NS = ns.NS_TBL['variant_ns']


VKIND_REL     = VerKind.REL
VKIND_SVNREV  = VerKind.SVN
VKIND_GITREV  = VerKind.GIT
VKIND_VARIANT = VerKind.VAR

VKIND_DEFAULT = VKIND_REL


DIR_MEM_LIMIT = 100

ast_ext  = '.ast'

compress_cmd = 'bzip2'
compress_ext = '.bz2'

UNKNOWN = 'UNKNOWN'

SRC_SVN = 'SVN'
SRC_URL = 'URL'

PARSER_INTERNAL = 'INTERNAL'
PARSER_EXTERNAL_C = 'EXTERNAL_C'



def get_proj_dir(proj_id, proj_dir=PROJECTS_DIR):
    d = os.path.abspath(os.path.join(proj_dir, proj_id))
    return d

def cmds_to_str(cmds):
    s = 'N/A'
    if cmds:
        l = ['"%s"' % x for x in cmds]
        s = ', '.join(l)
    return s


def pair_list_to_list(pair_list):
    l = []
    for pair in pair_list:
        for x in pair:
            if x not in l:
                l.append(x)
    return l


class Config(dp.base):
    def __init__(self):
        self.proj_id     = UNKNOWN
        self.proj_path   = None
        self.repo_url    = None
        self.repo_user   = None
        self.repo_passwd = None
        self.vkind       = VKIND_DEFAULT
        self.ranges      = []
        self.vers        = []
        self.versions    = []
        self.versionURIs = []
        self.build_cmds  = []
        self.nversions   = 0
        self.archive_ext = ''
        self.source      = SRC_SVN

        self.gitweb_proj_id = None
        self.hash_algo = HashAlgo.MD5

        self.retrieve_all  = False
        self.get_long_name = lambda x: x

        self.__parser      = PARSER_INTERNAL

        self.ssl_server_trust_prompt = None

        self.lang = UNKNOWN

        # for phylogeny 
        self.abbrev_list = []
        self.abbrev_tbl = {}
        self.inv_abbrev_tbl = {}

        self.single_file_mode = False

        self.vpairs = []
        self.vURIpairs = []

        self.sloc = None

        self.commits = []
        self.tag_table = {}

        self.include = []

        self.ver_tbl = None # short ver -> orig ver name


    def is_vkind_gitrev(self):
        return self.vkind == VKIND_GITREV

    def get_ver_dir(self, version):
        d = None

        if self.vkind == VKIND_GITREV:
            d = version
        else:
            d = os.path.join(self.proj_path, self.get_ver_dir_name(version))

        return d

    def get_ver_dir_name(self, version):
        dn = None
        if self.vkind == VKIND_REL:
            dn = version

        elif self.vkind == VKIND_VARIANT:
            dn = version

        elif self.vkind == VKIND_SVNREV:
            if self.nversions > DIR_MEM_LIMIT:
                rev = -1
                try:
                    rev = int(version)

                except ValueError:
                    self.error('illegal revision name: "%s"' % version)

                if rev >= 0:
                    ds = []
                    x = rev
                    nvers = self.nversions
                    while True:
                        x /= DIR_MEM_LIMIT
                        nvers /= DIR_MEM_LIMIT
                        ds.insert(0, str(x))
                        if nvers <= DIR_MEM_LIMIT:
                            break

                    ds.append(version)
                    dn = os.path.join(*ds)

            else:
                dn = version

        elif self.vkind == VKIND_GITREV:
            dn = version

        else:
            self.error('unsupported version kind: "%s"' % self.vkind)

        return dn


    def get_vkind(self, idx):
        return self.vkind

    def get_vkind_by_name(self, n):
        return self.vkind

    def get_vkey(self, idx):
        return self.vkind + '_' + self.versions[idx]

    def use_internal_parser(self):
        self.__parser = PARSER_INTERNAL

    def use_external_c_parser(self):
        self.__parser = PARSER_EXTERNAL_C

    def finalize(self, proj_dir=PROJECTS_DIR):
        if not self.proj_path:
            pid = self.proj_id
            if self.gitweb_proj_id:
                pid = self.gitweb_proj_id
            self.proj_path = get_proj_dir(pid, proj_dir=proj_dir)

        # for phylogeny
        if self.vers == []:
            self.vers = [n for (n, a) in self.abbrev_list] 

        self.abbrev_tbl = dict(self.abbrev_list)
        for (x, a) in self.abbrev_tbl.items():
            self.inv_abbrev_tbl[a.rstrip()] = x


        if self.vkind == VKIND_REL:
            self.versionNS = REL_INST_NS
            self.versions = [self.get_long_name(x) for x in self.vers]
            self.versionURIs = [REL_INST_NS + x for x in self.vers]

            if self.vpairs:
                self.vURIpairs = [(REL_INST_NS+x, REL_INST_NS+y) for (x, y) in self.vpairs]
                if not self.versions:
                    self.versions = pair_list_to_list(self.vpairs)
                    self.versionURIs = pair_list_to_list(self.vURIpairs)


        elif self.vkind == VKIND_VARIANT:
            self.versionNS = VARIANT_INST_NS
            self.versions = [self.get_long_name(x) for x in self.vers]
            self.versionURIs = [VARIANT_INST_NS+x for x in self.versions]

            if self.vpairs:
                self.vURIpairs = [(VARIANT_INST_NS+x, VARIANT_INST_NS+y) for (x, y) in self.vpairs]
                if not self.versions:
                    self.versions = pair_list_to_list(self.vpairs)
                    self.versionURIs = pair_list_to_list(self.vURIpairs)
            

        elif self.vkind == VKIND_SVNREV:
            self.versionNS = SVNREV_INST_NS
            self.versions = []
            for (st, ed) in self.ranges:
                for rev in xrange(st, ed+1):
                    srev = str(rev)
                    self.versions.append(srev)
                    self.versionURIs.append(SVNREV_INST_NS + srev)

            self.vers = self.versions

        elif self.vkind == VKIND_GITREV:
            self.versionNS = GITREV_INST_NS
            self.finalize_gitrev()

        else:
            self.error('version kind not supported: "%s"' % self.vkind)

        if len(self.vers) == 0:
            self.vers = self.versions

        if self.nversions == 0:
            self.nversions = len(self.versions)


    def finalize_gitrev(self):
        self.hash_algo = HashAlgo.GIT

        if not self.gitweb_proj_id:
            if self.repo_url:
                try:
                    self.gitweb_proj_id = self.repo_url.split('/')[-1]
                except:
                    self.gitweb_proj_id = re.sub(r'_git$', '.git', self.proj_id)

        self.versions = []

        try:

            repo = Git2.Repository(self.proj_path)

            if self.ranges:
                for (exc, frm) in self.ranges:

                    commits = []

                    for c in repo.walk_range(exc, frm):
                        commits.append(c)
                        self.commits.append(Git2.Commit(c))

                    oids = [c.id for c in commits]

                    for c in commits:
                        oid = c.id
                        h = oid.hex
                        grev1 = GITREV_INST_NS + h

                        self.versions.append(h)
                        self.versionURIs.append(grev1) 

                        parents = c.parents

                        if parents:
                            for poid in [p.id for p in parents[0:1]]: # ignore merges
                                if poid in oids:
                                    self.debug('parent of {0}: {1}'.format(shorten_sha(poid.hex),
                                                                           shorten_sha(h)))
                                    ph = poid.hex
                                    grev0 = GITREV_INST_NS + ph

                                    self.vpairs.append((ph, h))
                                    self.vURIpairs.append((grev0, grev1))

                self.vers = [shorten_sha(v) for v in self.versions]

            else:
                if not self.vers:
                    self.vers = ['HEAD']

                if len(self.vers) == 1:
                    orig_ver = self.vers[0]
                    self.commits = []
                    self.commits = repo.list_info_from(orig_ver, self.nversions)
                    self.versions = []
                    self.versionURIs = []
                    for c in self.commits:
                        h = c.oid.hex
                        grev = GITREV_INST_NS + h
                        self.versions.append(h)
                        self.versionURIs.append(grev)

                    self.vers = [shorten_sha(v) for v in self.versions]
                    self.nversions = len(self.vers)

                    if self.nversions == 1:
                        self.ver_tbl = { self.vers[0] : orig_ver }

                else:
                    self.commits = []
                    self.ver_tbl = {}
                    for v in self.vers:
                        try:
                            c = repo.get_commit_info(v)
                            self.commits.append(c)

                            h = c.oid.hex
                            self.versions.append(h)
                            self.versionURIs.append(GITREV_INST_NS+h)

                            if repo.is_tag(v):
                                self.tag_table[h] = v

                            self.ver_tbl[shorten_sha(h)] = v

                        except Exception as e:
                            self.warning(str(e))

            
        except Exception as e:
            self.warning(str(e))


    def check(self):
        if self.vkind != VKIND_GITREV:
            for v in self.versions:
                p = self.get_ver_dir(v)
                if not os.path.exists(p):
                    self.warning('does not exist: "%s"' % p)

    def get_index(self, ver):
        for i in xrange(self.nversions):
            if self.versions[i] == ver or self.vers[i] == ver:
                return i
        return -1

    def __str__(self):
        fmt =  'proj_id    : "%s"\n'
        fmt += 'proj_path  : "%s"\n'
        fmt += 'repo_url   : "%s"\n'
        fmt += 'vkind      : %s\n'
        fmt += 'num vers   : %d\n'
        fmt += 'num vpairs : %d\n'
        fmt += 'parser     : %s\n' 
        fmt += 'lang       : %s\n' 

        s = fmt % (self.proj_id, 
                   self.proj_path,
                   self.repo_url, 
                   self.vkind,
                   self.nversions,
                   len(self.vpairs),
                   self.__parser,
                   self.lang)

        if self.sloc:
            s += 'SLOC       : %s\n' % self.sloc
    
        if self.__parser == PARSER_EXTERNAL_C:
            s += 'build_cmds : %s\n' % (cmds_to_str(self.build_cmds))

        if self.vpairs:
            s += 'vpairs:\n'
            for (pv, v) in self.vpairs:
                s += '  %s - %s\n' % (pv, v)
        else:
            s += 'vers:\n'
            for v in self.vers:
                s += '  %s\n' % v

        if self.ver_tbl:
            s += 'ver_tbl:\n'
            for vo in self.ver_tbl.items():
                s += '  %s -> %s\n' % vo
            

        return s
