#!/usr/bin/env python3


'''
  find_change_patterns.py

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
import re
import time
import sys
import random
import locale
from urllib.request import pathname2url
from datetime import timedelta, tzinfo, datetime
import json
from xml.sax.saxutils import escape
import logging

from .siteconf import CCA_HOME, PROJECTS_DIR_NAME
from . import project, sparql
from .factextractor import compo_join, P_TYPE, make_literal
from .ns import FB_NS, CHG_NS, NS_TBL, PREFIX_TBL
from .sparql import get_localname
from .virtuoso import VIRTUOSO_PW, VIRTUOSO_PORT
from .common import setup_logger

from cca import factutil
from cca.factutil.entity import SourceCodeEntity
from cca.factutil.fact import Fact
from cca.factutil.rdf import Resource, Predicate


logger = logging.getLogger()

DEFAULT_FACT_OUTPUT_DIR = '/opt/virtuoso/tmp/chgpat/<PROJ_ID>'


ENT_PAIR_NS = NS_TBL['entpair_ns']
CHGPAT_INST_NS = NS_TBL['chgpat_ns']

LOCALE = 'en_US'

KEY_VAR_NAME = 'KEY'
GROUP_VAR_NAME = 'GROUP'
NAME_SEPARATOR = ';'
GROUP_SEPARATOR = '/'

random.seed('DIFFTS-FIND-CHANGE-PATTERNS')

TD_ZERO = timedelta(0)


class JST(tzinfo):
    def utcoffset(self, dt):
        return timedelta(hours=9)

    def dst(self, dt):
        return TD_ZERO

    def tzname(self, dt):
        return 'JST'


class TZ(tzinfo):
    def __init__(self, ofs):
        self.__utcoffset = timedelta(minutes=ofs)

    def utcoffset(self, dt):
        return self.__utcoffset

    def dst(self, dt):
        return TD_ZERO


escape_tbl = {
    '"': '&quot;',
    "'": '&apos;',
    '<': '&lt;',
    '>': '&gt;',
}


def html_escape(s):
    return escape(s, escape_tbl)


def jsondumps(obj):
    return html_escape(json.dumps(obj, separators=(',', ':')))


ENUM_QUERY_DIR = os.path.join(CCA_HOME, 'queries', 'enum')
Q_ENUM_MAPPING_CHG = 'enumerate_mapping_changes.rq'
Q_ENUM_ADDITION = 'enumerate_additions.rq'
Q_ENUM_REMOVAL = 'enumerate_removals.rq'

# Q_ENUM_FILE = 'enumerate_files.rq'


HTML_HEAD = '''<html>
<head><title>%(proj_id)s</title>
<link rel="stylesheet" type="text/css" href="%(url_base_path)s/demo.css"/>
</head>
<body>

<script type="text/javascript">
  function formAutoSubmit () {
    var frag = window.location.hash;
    if (frag[0] == "#")
      frag = frag.slice(1);
    if (frag) {
      var frm = document.getElementById(frag);
      if (frm) {
        frm.target = "_self";
        frm.submit();
      }
    }
  }
  window.onload = formAutoSubmit;
</script>

<div class="top">
'''

HTML_TAIL = '''</div></body></html>
'''

FRAME_PAGE = '''<html>
<head><title>%(proj_id)s</title>
<link rel="stylesheet" type="text/css" href="%(url_base_path)s/demo.css"/>
</head>
<body>
<h1>%(proj_id)s</h1>
<iframe id="navi" src="%(summary)s" align="left" width="40%%" height="90%%" frameborder="0">
<a href="%(summary)s">Summary</a><br/>
</iframe>
<iframe id="content" name="right" src="%(first)s" align="right" width="60%%" height="90%%" frameborder="0">
</iframe>
<div class="footer">
&copy; 2013-%(year)s Codinnum Software Lab
</div>
</body></html>
'''

HTML_ITEM = '''<li>
[%(count)d] <span class="name">%(rname)s</span><br/>
%(others)s
%(applet)s
</li>
'''

APPLET = '''
<table class="noframe" border="0" cellspacing="0" cellpadding="0"><tr><td>
<span class="variable">%(var0)s</span> -> <span class="variable">%(var1)s</span> (%(startl0)dL,%(startc0)dC - %(startl1)dL,%(startc1)dC)
</td><td>
<form id="%(form_id)s" method="POST" action="%(url_base_path)s/cgi-bin/openviewer" target="_blank">
<input type="hidden" name="proj_id" value="%(proj_id)s">
<input type="hidden" name="hash_algo" value="%(algo)s">
<input type="hidden" name="ver0" value="%(ver0)s">
<input type="hidden" name="ver1" value="%(ver1)s">
<input type="hidden" name="path0" value="%(path0)s">
<input type="hidden" name="path1" value="%(path1)s">
<input type="hidden" name="src0" value="%(src0)s">
<input type="hidden" name="src1" value="%(src1)s">
<input type="hidden" name="startl0" value="%(startl0)d">
<input type="hidden" name="startl1" value="%(startl1)d">
<input type="hidden" name="startc0" value="%(startc0)d">
<input type="hidden" name="startc1" value="%(startc1)d">
<input type="hidden" name="desc" value="%(desc)s">
<input type="hidden" name="ess0" value="%(ess0)s">
<input type="hidden" name="ess1" value="%(ess1)s">
<input type="hidden" name="ess01" value="%(ess01)s">
<input type="hidden" name="highlight0" value="%(hilit0)s">
<input type="hidden" name="highlight1" value="%(hilit1)s">
<input type="hidden" name="vpid" value="%(vpid)s">
<input type="hidden" name="form_id" value="%(form_id)s">
<input type="button" style="background-color:#c0c0c0" value="Open Viewer" onClick="submit()">
</form></td>
</tr></table>
'''

HTML_TBL = '''<tr align="center">
<td>%(proj_id)s</td>
<td>%(nversions)s</td>
<td class="right">%(sloc)s(%(avg_sloc)s)</td>
<td class="right">%(ntriples)s</td>
<td><a href="%(html)s" target="_blank">%(npatterns)s</a></td>
</tr>
'''

TRIPLE_COUNT_QUERY = '''
SELECT DISTINCT (COUNT(*) AS ?count)
FROM <%(graph)s>
WHERE {
  ?s ?p ?o .
}
'''

VER_QUERY = '''
PREFIX ver:  <%(ver_ns)s>
SELECT DISTINCT ?v
FROM <%%(graph)s>
WHERE {
  <%%(fent)s> ver:version ?v .
}
''' % NS_TBL

VER_PAIR_QUERY = '''
PREFIX ver:  <%(ver_ns)s>
SELECT DISTINCT ?v ?v_
FROM <%%(graph)s>
WHERE {
  <%%(fent0)s> ver:version ?v .
  <%%(fent1)s> ver:version ?v_ .
  ?v ver:next ?v_ .
}
''' % NS_TBL

SRCTREE_PAIR_QUERY = '''
PREFIX chg:  <%(chg_ns)s>
PREFIX ver:  <%(ver_ns)s>
SELECT DISTINCT ?srcpair
FROM <%%(graph)s>
WHERE {
  ?srcpair a chg:SourceTreePair ;
           chg:originalSourceTree/ver:version <%%(ver0)s> ;
           chg:modifiedSourceTree/ver:version <%%(ver1)s> .
}
''' % NS_TBL

SRCTREE_QUERY = '''
PREFIX src:  <%(src_ns)s>
PREFIX ver:  <%(ver_ns)s>
SELECT DISTINCT ?src
FROM <%%(graph)s>
WHERE {
  ?src a src:SourceTree ;
       ver:version <%%(ver)s> .
}
''' % NS_TBL

PATH_QUERY = '''
PREFIX src:  <%(src_ns)s>
PREFIX ver:  <%(ver_ns)s>
PREFIX gsrc: <%(gsrc_ns)s>
SELECT DISTINCT ?loc
FROM <%%(graph)s>
WHERE {
  ?file a src:File ;
        ver:version ?ver ;
        src:location ?loc .

  ?g gsrc:location ?file ;
     gsrc:location ?loc ;
     ver:version ?ver .

  FILTER (?file = <%%(fent)s>)
  FILTER (?ver = <%%(ver)s>)
}
''' % NS_TBL

ALL_PATH_QUERY = '''
PREFIX src:  <%(src_ns)s>
SELECT DISTINCT ?loc
FROM <%%(graph)s>
WHERE {
  ?file a src:File ;
        src:location ?loc .

  FILTER (?file = <%%(fent)s>)
}
''' % NS_TBL

GIT_COMMIT_INFO_QUERY = '''
PREFIX git: <%(git_ns)s>
SELECT DISTINCT ?cmtr ?date ?mes ?ofs
FROM <%%(graph)s>
WHERE {
  <%%(ver)s> git:message ?mes ;
             git:committer ?cmtr ;
             git:commitDate ?date ;
             git:commitDateOffset ?ofs .
}
''' % NS_TBL

ENUM_FILES_QUERY = '''
PREFIX src: <%(src_ns)s>
SELECT DISTINCT ?ent
FROM <%%(graph)s>
WHERE {
  ?ent a src:File .
}
''' % NS_TBL


class QueryNotFound(Exception):
    def __init__(self, q):
        self.query = q


locale.setlocale(locale.LC_ALL, LOCALE)


def format_num(n):
    s = '-'
    if n:
        s = locale.format('%d', n, grouping=True)
    return s


def capitalize(s):
    if s.isupper():
        return s
    else:
        return s.capitalize()


def fname_to_title(fname):
    (name, ext) = os.path.splitext(fname)
    ws = name.split('_')
    title = ' '.join([capitalize(w) for w in ws])
    return title


def mkfilepair(fent1, fent2):
    id1 = fent1.get_local_name()
    id2 = fent2.get_local_name()
    if id1 is None:
        id1 = 'null'
    if id2 is None:
        id2 = 'null'
    uri = ENT_PAIR_NS + compo_join(id1, id2)
    logger.debug('uri=%s' % uri)
    return Resource(uri=uri)


def mkchgpat(ent1, ent2, _chg):
    chg = _chg.lower().replace('"', '').replace(' ', '_')
    cid = compo_join(ent1.get_local_name(),
                     ent2.get_local_name(),
                     chg)

    uri = CHGPAT_INST_NS + cid
    logger.debug('uri=%s' % uri)
    return Resource(uri=uri)


def make_literals(str_set):
    li = []
    for s in str_set:
        for s0 in s.split(NAME_SEPARATOR):
            li.append(make_literal(s0))
    return li


def str_set_to_str(ss):
    return NAME_SEPARATOR.join(sorted([html_escape(s) for s in ss]))


zero_metric = {'copts': '', 'value': 0}


class Predicates(object):
    def __init__(self):
        self.c_filepair = Resource(uri=CHG_NS+'FilePair')
        self.p_orig_file = Predicate(CHG_NS, 'originalFile')
        self.p_modi_file = Predicate(CHG_NS, 'modifiedFile')

        self.chgpat_ns = None
        self.p_filepair = None
        self.p_chgpat = None


class FactExtractor(object):
    def __init__(self, graph, conf, method='odbc',
                 pw=VIRTUOSO_PW, port=VIRTUOSO_PORT):
        self._graph = graph
        self._conf = conf
        self.metrics = ""
        self.metrics_reverse = False

        self._sparql = sparql.get_driver(method, pw=pw, port=port)

    def get_metric(self, x, y):
        return zero_metric

    def extract_srctree_fact(self, fact, ent0, ent1, ver0, ver1):
        pass

    def extract_file_fact(self, fact, ent0, ent1, ver0, ver1):
        pass

    def get_other_info(self, ver0, ver1):
        return {}

    def get_git_commit_info(self, ver0, ver1):
        info = {}
        if self._conf.is_vkind_gitrev():
            q = GIT_COMMIT_INFO_QUERY % {'graph': self._graph,
                                         'ver': ver1}

            for qvs, row in self._sparql.query(q):
                info['Committer'] = row['cmtr']
                info['Message'] = '%s' % row['mes']
                try:
                    ts = int(row['date'])
                    ofs = int(row['ofs'])
                    dt = datetime.fromtimestamp(ts, TZ(ofs))

                    jst_dt = dt.astimezone(JST())

                    # info['Date'] = dt.isoformat(' ')
                    info['Date'] = jst_dt.isoformat(' ')
                except Exception as e:
                    logger.warning(str(e))

        return info


class Finder(object):
    def __init__(self, qdir, queries, base_dir, proj_id,
                 predicate_tbl=None,
                 limit=None,
                 lang=None,
                 extra_fact_extractor=None,
                 conf=None,
                 method='odbc',
                 pw=VIRTUOSO_PW,
                 port=VIRTUOSO_PORT):

        self._query_dir = qdir
        self._queries = queries
        self._base_dir = base_dir
        self._proj_id = proj_id
        self._graph_uri = FB_NS + proj_id
        self._sparql = sparql.get_driver(method, pw=pw, port=port)
        self._result = {}  # lang -> (ver * ver) -> (fid * fid) -> change_pat_name ->  (ent * ent) list
        self._cache_base_dir = os.path.join(base_dir, 'work.diffts',
                                            proj_id+'.fact')
        if conf is None:
            self._conf = project.get_conf(proj_id)
        else:
            self._conf = conf
        self._hash_algo = self._conf.hash_algo

        self._vi_tbl = {}
        for i in range(self._conf.nversions):
            self._vi_tbl[self._conf.versions[i]] = i

        if limit is not None and limit > 0:
            self._limit = limit
        else:
            self._limit = None

        self._lang = lang

        self._latest_version_cache = {}
        self._earliest_version_cache = {}
        self._version_pair_cache = {}
        self._version_pairs_cache = {}

        self._extra_ent_pairs_tbl = {}  # ent * ent -> var * var -> (ent * ent) list

        self._others_tbl = {}  # ent * ent -> var -> val

        self._ess_tbl_rm = {}  # ent * ent -> removed -> ent list
        self._ess_tbl_ad = {}  # ent * ent -> added -> ent list
        self._ess_tbl_mp = {}  # ent * ent -> mapped -> ent list

        self._prim_chg_tbl = {}  # ver * ver -> fid * fid -> (ent * ent * chg) list
        self._prim_chg_count_tbl = {}  # ver * ver -> fid * fid -> int

        self._coverage_cache_f = {}
        self._coverage_cache_v = {}

        self._fent_tbl = {}  # fid -> fent

        self._src_path_tbl = {}  # (fid * verURI) -> path

        self._predicate_tbl = predicate_tbl

        self._extra_fact_extractor = None
        if extra_fact_extractor:
            self._extra_fact_extractor = extra_fact_extractor(self._graph_uri,
                                                              self._conf,
                                                              method=method,
                                                              pw=pw, port=port)

        self._metric_tbl = {}

        self._change_enumeration = False

    def get_ver_idx(self, v):
        return self._vi_tbl[v]

    def get_ver_name(self, verURI):
        return self._conf.vers[self.get_vindex(verURI)]

    def get_ver_dir(self, verURI):
        return self._conf.get_ver_dir(self._conf
                                      .versions[self.get_vindex(verURI)])

    def get_ver_dir_r(self, verURI):
        v = self._conf.versions[self.get_vindex(verURI)]
        vdn = self._conf.get_ver_dir_name(v)
        return os.path.join(PROJECTS_DIR_NAME, self._proj_id, vdn)

    def get_abbrev_ver(self, lver):
        av = lver
        try:
            i = self.get_ver_idx(lver)
            av = self._conf.vers[i]
        except Exception:
            # logger.warning('cannot get abbreviation for "%s"' % lver)
            pass
        return av

    def disable_change_enumeration(self):
        self._change_enumeration = False

    def enable_change_enumeration(self):
        self._change_enumeration = True

    def get_predicates(self, lang):
        ps = None
        if self._predicate_tbl:
            try:
                ps = self._predicate_tbl[lang]
            except KeyError:
                pass
        return ps

    def extract_fact(self, lang):
        ps = self.get_predicates(lang)
        b = ps is not None
        return b

    def mkchgpatpred(self, lang, s):
        ps = self.get_predicates(lang)
        p = Predicate(ps.chgpat_ns, s)
        return p

    def mkchgtype(self, lang, _chg):
        chg = _chg
        qst = chg.find('"')
        qed = chg.rfind('"')
        if qst >= 0 and qed >= 0:
            chg = _chg[qed+1:]

        tn = ''.join([s.capitalize() for s in chg.split(' ')])

        ps = self.get_predicates(lang)
        uri = ps.chgpat_ns + tn

        r = Resource(uri=uri)

        return r

    def get_query(self, lang, name, force_per_ver=False):
        query = None
        per_ver = False
        path = None
        if lang:
            path = os.path.join(self._query_dir, lang, name)
        else:
            path = os.path.join(ENUM_QUERY_DIR, name)

        try:
            with open(path, 'r') as f:
                q = f.read()

                if force_per_ver:
                    logger.debug('forcing the query to be executed for each version pairs')
                    q = re.sub(r'\?ver', '?VER', q)

                _query = re.sub(r'WHERE', ('WHERE {\nGRAPH <%s>' % self._graph_uri),
                                q, count=1, flags=re.IGNORECASE).rstrip('\n ;')
                query = '}}'.join(_query.rsplit('}', 1))

                if q.find('?VER') > -1 and q.find('?VER_') > -1:
                    per_ver = True

        except Exception:
            raise QueryNotFound(path)
            # logger.error(str(e))

        return query, per_ver

    def add_ver_filter(self, q, ver0, ver1):
        # q0 = q.replace('?VER_', '<%s>' % ver1)
        # query = q0.replace('?VER', '<%s>' % ver0)

        query = q.replace('{', '{\nFILTER (?VER = <%s> && ?VER_ = <%s>)' % (ver0, ver1), 1)

        # query = re.sub(r'(\?VER\s+ver:version\s+\?VER_\s*\.)',
        #                r'\1\nFILTER (?VER = <%s> && ?VER_ = <%s>)\n' % (ver0, ver1),
        #                q)
        return query

    def get_version_pair(self, ent0, ent1):
        fent0 = ent0
        fent1 = ent1

        if not ent0.is_file():
            fent0 = SourceCodeEntity(file_id=ent0.get_file_id())

        if not ent1.is_file():
            fent1 = SourceCodeEntity(file_id=ent1.get_file_id())

        try:
            return self._version_pair_cache[(fent0, fent1)]
        except KeyError:
            pass

        q = VER_PAIR_QUERY % {'fent0': fent0.get_uri(),
                              'fent1': fent1.get_uri(),
                              'graph': self._graph_uri,
                              }
        pair = None
        idx = sys.maxsize

        for qvs, row in self._sparql.query(q):
            v0 = row['v']
            v = get_localname(v0)
            try:
                i = self.get_ver_idx(v)
                if i < idx:
                    idx = i
                    pair = (v0, row['v_'])
            except Exception:
                logger.error('cannot get index for version "%s"' % v)

        if pair is None:
            logger.warning('cannot get version pair for "{}" and "{}"'
                           .format(ent0, ent1))
        else:
            self._version_pair_cache[(fent0, fent1)] = pair

        return pair

    def get_version_pairs(self, ent0, ent1):
        fent0 = ent0
        fent1 = ent1

        if not ent0.is_file():
            fent0 = SourceCodeEntity(file_id=ent0.get_file_id())

        if not ent1.is_file():
            fent1 = SourceCodeEntity(file_id=ent1.get_file_id())

        try:
            return self._version_pairs_cache[(fent0, fent1)]
        except KeyError:
            pass

        q = VER_PAIR_QUERY % {'fent0': fent0.get_uri(),
                              'fent1': fent1.get_uri(),
                              'graph': self._graph_uri,
                              }

        pairs = []

        for qvs, row in self._sparql.query(q):
            pairs.append((row['v'], row['v_']))

        if pairs == []:
            logger.warning('cannot get version pair for "{}" and "{}"'
                           .format(ent0, ent1))
        else:
            self._version_pairs_cache[(fent0, fent1)] = pairs

        return pairs

    def get_srctree_pair_ent(self, ver0, ver1):
        q = SRCTREE_PAIR_QUERY % {'ver0': ver0,
                                  'ver1': ver1,
                                  'graph': self._graph_uri
                                  }

        li = []

        for qvs, row in self._sparql.query(q):
            r = Resource(uri=row['srcpair'])
            li.append(r)

        n = len(li)
        if n == 0:
            logger.warning('SourceTreePair not found for "{}" and "{}"'
                           .format(ver0, ver1))
        elif n == 1:
            return li[0]
        else:
            logger.warning('multiple SourceTreePair found for "{}" and "{}":\n{}'
                           .format(ver0, ver1, '\n'.join(li)))
            return li[0]

    def get_srctree_ent(self, ver):
        q = SRCTREE_QUERY % {'ver': ver,
                             'graph': self._graph_uri
                             }

        li = []

        for qvs, row in self._sparql.query(q):
            r = Resource(uri=row['src'])
            li.append(r)

        n = len(li)
        if n == 0:
            logger.warning(f'SourceTree not found for "{ver}"')
        elif n == 1:
            return li[0]
        else:
            logger.warning('multiple SourceTree found for "{}":\n{}'
                           .format(ver, '\n'.join(li)))
            return li[0]

    def count_triples(self):
        q = TRIPLE_COUNT_QUERY % {'graph': self._graph_uri}
        count = None
        for qvs, row in self._sparql.query(q):
            count = row['count']
        return count

    def _get_version(self, ent, direction):
        fent = SourceCodeEntity(file_id=ent.get_file_id())

        try:
            if direction >= 0:  # latest
                return self._latest_version_cache[fent]
            elif direction < 0:  # earliest
                return self._earliest_version_cache[fent]
        except KeyError:
            pass

        q = VER_QUERY % {'fent': fent.get_uri(), 'graph': self._graph_uri}

        ver = None
        idx = None
        for qvs, row in self._sparql.query(q):
            v = get_localname(row['v'])
            try:
                i = self.get_ver_idx(v)
            except Exception:
                logger.error(f'cannot get index for version "{v}"')

            if ver is None:
                ver = row['v']
                idx = i
            elif direction >= 0:  # latest
                if i > idx:
                    ver = row['v']
                    idx = i
                else:
                    pass
            elif direction < 0:  # earliest
                if i < idx:
                    ver = row['v']
                    idx = i
                else:
                    pass

        if ver is None:
            logger.warning(f'cannot get version of "{ent}"')
        else:
            if direction >= 0:  # latest
                self._latest_version_cache[fent] = ver
            elif direction < 0:  # earliest
                self._earliest_version_cache[fent] = ver

        return ver

    def get_latest_version(self, ent):
        return self._get_version(ent, 1)

    def get_earliest_version(self, ent):
        return self._get_version(ent, -1)

    def get_cache_path(self, fd0, fd1):
        p = os.path.join(self._cache_base_dir, fd0[0:2], fd0+'-'+fd1)
        return p

    def get_diff_url(self, fid0, fid1):
        fd0 = fid0.get_value()
        fd1 = fid1.get_value()
        p = os.path.join(self.get_cache_path(fd0, fd1), 'diff.json')
        u = pathname2url(p)
        return u

    def path_ok(self, path):
        cond = True
        if self._conf.include:
            cond = any(path.startswith(x) for x in self._conf.include)
        # logger.warning('%s --> %s' % (path, cond))
        return cond

    def get_source_path(self, fid, verURI):
        path = None

        if fid is None:
            return path

        try:
            path = self._src_path_tbl[(fid, verURI)]
            return path
        except KeyError:
            pass

        fent = SourceCodeEntity(file_id=fid)
        q = PATH_QUERY % {'fent': fent.get_uri(),
                          'ver': verURI,
                          'graph': self._graph_uri,
                          }
        locs = []
        for qvs, row in self._sparql.query(q):
            loc = row['loc']
            if self.path_ok(loc):
                locs.append(loc)

        n = len(locs)
        if n == 1:
            path = locs[0]
        elif n > 1:
            logger.warning('multiple paths found for "{}"@"{}":\n{}'
                           .format(fid.encode(),
                                   get_localname(verURI),
                                   '\n'.join(locs)))
            path = locs[0]
        else:
            q = ALL_PATH_QUERY % {'fent': fent.get_uri(),
                                  'graph': self._graph_uri,
                                  }
            locs = []
            for qvs, row in self._sparql.query(q):
                loc = row['loc']
                if self.path_ok(loc):
                    locs.append(loc)

            n = len(locs)
            if n == 1:
                path = locs[0]
            else:
                if n > 1:
                    logger.warning('multiple paths found for "{}"@"{}":\n{}'
                                   .format(fid.encode(),
                                           get_localname(verURI),
                                           '\n'.join(locs)))

                fd = fid.get_value()
                vkey = self._conf.get_vkey(self.get_vindex(verURI))
                src = os.path.join(self._cache_base_dir, fd[0:2], fd, 'source.'+vkey)
                try:
                    f = open(src, 'r')
                    if self._conf.is_vkind_gitrev():
                        path = f.readline().rstrip()
                    else:
                        p = os.path.realpath(f.readline().rstrip())
                        path = p.replace(self.get_ver_dir(verURI)+os.sep, '')

                except Exception as e:
                    # logger.warning(str(e))
                    logger.debug(str(e))

        logger.debug('path="%s"' % path)
        if path is None and not self._conf.include:
            logger.warning('path not found for "{}"@"{}"'
                           .format(fid.encode(), get_localname(verURI)))

        self._src_path_tbl[(fid, verURI)] = path

        return path

    def check_path(self, ent0, ent1, ver0, ver1, per_ver):
        b = False
        if self._conf.include:
            ver_pairs = [(ver0, ver1)]
            if not per_ver:
                ver_pairs = self.get_version_pairs(ent0, ent1)

            if ver_pairs:
                for (v0, v1) in ver_pairs:
                    path0 = self.get_source_path(ent0.get_file_id(), v0)
                    path1 = self.get_source_path(ent1.get_file_id(), v1)
                    if path0 or path1:
                        b = True
                        break

        else:
            b = True

        return b

    def get_source_url(self, fid, verURI, path=None):
        url = None

        if fid is None:
            return url

        p = path
        if p is None:
            p = self.get_source_path(fid, verURI)

        if self._conf.is_vkind_gitrev():
            fd = fid.get_value()
            if self._conf.gitweb_proj_id:
                url = '/gitweb/?p={};a=blob_plain;f={};h={};hb={}'\
                    .format(self._conf.gitweb_proj_id,
                            p,
                            fd,
                            get_localname(verURI))
            else:
                url = pathname2url(p)
                logger.warning('cannot get source url: fd="{}" verURI="{}"'
                               .format(fd, verURI))
        else:
            try:
                if p:
                    url = os.path.join(self.get_ver_dir_r(verURI), p)
            except Exception as e:
                logger.warning('cannot get source URL for fid={} verURL={} path={}: {}'
                               .format(fid, verURI, path, e))
        return url

    def get_hilit(self, r):
        return '%d:%d' % (r.get_start_offset(), r.get_end_offset())

    def setup_metric_tbl(self, vpairs):
        self._metric_tbl = {}
        if self._extra_fact_extractor:
            for (x, y) in vpairs:
                m = self._extra_fact_extractor.get_metric(get_localname(x),
                                                          get_localname(y))
                if m:
                    self._metric_tbl[(x, y)] = m
                else:
                    self._metric_tbl[(x, y)] = zero_metric

            no_metric = True
            for (k, m) in self._metric_tbl.items():
                if m != zero_metric:
                    no_metric = False
                    break

            if no_metric:
                self._metric_tbl = {}

    def sort_ver_pairs(self, vpairs):
        if vpairs:
            if self._metric_tbl:
                r = self._extra_fact_extractor.metrics_reverse
                vpairs.sort(key=lambda p: self._metric_tbl[p]['value'],
                            reverse=r)

            else:
                uris = self._conf.versionURIs
                vpairs.sort(key=lambda x_y:
                            (uris.index(x_y[0]), uris.index(x_y[1])))
                # vpairs.sort(key=lambda (x,y): self.get_ver_idx(x))

    def _show_stat(self, f, count_tbl, cov):
        change_pats = count_tbl.keys()
        total = 0
        for r in change_pats:
            total += count_tbl[r]
        nchange_pats = len(change_pats)

        count_items = list(count_tbl.items())
        count_items.sort(key=lambda x: x[1], reverse=True)

        f.write('<span class="term">Found:</span> {} change patterns ({} instances)<br/>\n'
                .format(nchange_pats, total))

        if cov:
            f.write('<span class="term">Coverage:</span> {}/{}={:3.2f}<br/>\n'
                    .format(*cov))

        f.write('<span class="term">Instances:</span><br/>\n')

        f.write('<table class="noframe" border="0" cellspacing="0">\n')
        for (change_pat, c) in count_items:
            p = float(c) / float(total) * 100.0
            f.write('<tr><td><span class="name">{}</span></td><td align="right">{}</td>'
                    .format(change_pat, c))
            f.write('<td align="right">(%3.2f%%)</td></tr>\n' % p)
        f.write('</table>\n')

    def make_applet_html(self, vpid, form_id, var0, var1, ent0, ent1,
                         ver0, ver1, desc='{}', ess0='[]', ess1='[]',
                         ess01='[]', url_base_path='..'):
        fid0 = ent0.get_file_id()
        fid1 = ent1.get_file_id()

        path0 = self.get_source_path(fid0, ver0)
        path1 = self.get_source_path(fid1, ver1)

        if path0 is None and path1 is None:
            return ''

        url0 = self.get_source_url(fid0, ver0, path=path0)
        url1 = self.get_source_url(fid1, ver1, path=path1)

        if url0 is None or url1 is None:
            return ''

        else:
            aver0 = self.get_abbrev_ver(get_localname(ver0))
            aver1 = self.get_abbrev_ver(get_localname(ver1))

            data = {
                'proj_id': self._proj_id,
                # 'cache': urlname2url(self._cache_base_dir),
                # 'diff': self.get_diff_url(fid0, fid1),
                'algo': self._hash_algo,
                'var0': var0,
                'var1': var1,
                'ver0': aver0,
                'ver1': aver1,
                'path0': path0,
                'path1': path1,
                'src0': url0,
                'src1': url1,
                'vpid': vpid,
                'form_id': form_id,
                'url_base_path': url_base_path,
            }

            r0 = ent0.get_range()
            r1 = ent1.get_range()

            line0 = r0.get_start_line()
            line1 = r1.get_start_line()

            col0 = r0.get_start_col()
            col1 = r1.get_start_col()

            data['startl0'] = line0
            data['startl1'] = line1

            data['startc0'] = col0
            data['startc1'] = col1

            data['hilit0'] = self.get_hilit(r0)
            data['hilit1'] = self.get_hilit(r1)

            data['desc'] = desc
            data['ess0'] = ess0
            data['ess1'] = ess1
            data['ess01'] = ess01

            applet = APPLET % data

            return applet

    def get_total_coverage(self):
        total = 0
        identified = 0

        for (ver0, ver1) in self._prim_chg_count_tbl.keys():
            (i, t, c) = self.get_coverage_v(ver0, ver1)
            identified += i
            total += t

        coverage = None

        if total > 0:
            coverage = float(identified) / float(total)

        return (identified, total, coverage)

    def get_coverage_v(self, ver0, ver1):
        try:
            return self._coverage_cache_v[(ver0, ver1)]
        except KeyError:

            total = 0
            identified = 0

            try:
                fd_tbl = self._prim_chg_count_tbl[(ver0, ver1)]
                for (fid0, fid1) in fd_tbl.keys():
                    (i, t, c) = self.get_coverage_f(ver0, ver1, fid0, fid1)
                    identified += i
                    total += t
            except KeyError:
                pass

            coverage = None

            if total > 0:
                coverage = float(identified) / float(total)

            res = (identified, total, coverage)

            self._coverage_cache_v[(ver0, ver1)] = res

            return res

    def get_coverage_f(self, ver0, ver1, fid0, fid1):
        try:
            return self._coverage_cache_f[(ver0, ver1, fid0, fid1)]
        except KeyError:

            total = 0
            unidentified = 0

            try:
                total = self._prim_chg_count_tbl[(ver0, ver1)][(fid0, fid1)]
            except KeyError:
                pass

            try:
                unidentified = len(self._prim_chg_tbl[(ver0, ver1)][(fid0, fid1)])
            except KeyError:
                pass

            coverage = None

            identified = total - unidentified

            if total > 0:
                coverage = float(identified) / float(total)

            res = (identified, total, coverage)

            self._coverage_cache_f[(ver0, ver1, fid0, fid1)] = res

            return res

    def reduce_chgs(self, chgs):
        prune = set()
        graft = set()
        for chg in chgs:
            (ent0, ent1, ch, cat) = chg
            if ch in ('grafted onto', 'weak addition'):
                graft.add(chg)

            elif ch in ('pruned from', 'weak removal'):
                prune.add(chg)

        logger.debug('|prune|=%d |graft|=%d' % (len(prune), len(graft)))

        reduced = set()

        for chg in chgs:
            (ent0, ent1, ch, cat) = chg
            b = True
            for (e0, e1, c, ct) in prune:
                if ch in ('deleted from',
                          'pruned from',
                          'weak removal') \
                          and e0.contains(ent0) and e0 != ent0:
                    b = False
                    break
            if b:
                for (e0, e1, c, ct) in graft:
                    if ch in ('inserted into',
                              'grafted onto',
                              'weak addition') \
                              and e1.contains(ent1) and e1 != ent1:
                        b = False
                        break

            # if c in ('weak addition', 'weak removal'):
            #     b = False

            if b:
                reduced.add(chg)
            else:
                prune.discard(chg)
                graft.discard(chg)

        logger.debug('|prune|={} |graft|={} |reduced|={}'
                     .format(len(prune), len(graft), len(reduced)))

        return (reduced | prune | graft)

    def dump(self, outdir, foutdir=None, url_base_path='..'):

        if not os.path.exists(outdir):
            logger.warning('creating "%s"...' % outdir)
            os.makedirs(outdir)

        fact = Fact(PREFIX_TBL)

        html_dir = os.path.join(outdir, self._proj_id)

        if not os.path.exists(html_dir):
            logger.warning('creating "%s"...' % html_dir)
            os.makedirs(html_dir)

        html_path = os.path.join(html_dir, 'index.html')

        path_summary = os.path.join(html_dir, 'summary.html')

        f_summary = open(path_summary, 'w')

        sloc = '-'
        try:
            sloc = format_num(self._conf.sloc)
        except Exception:
            pass
        avg_sloc = '-'
        try:
            avg_sloc = format_num(self._conf.sloc / self._conf.nversions)
        except Exception:
            pass

        html_tbl_data = {'proj_id': self._proj_id,
                         'sloc': sloc,
                         'avg_sloc': avg_sloc,
                         'html': html_path,
                         'nversions': format_num(self._conf.nversions),
                         }

        count_tbl = {}

        for lang in self._result.keys():
            ver_tbl = self._result[lang]
            for ver_pair in ver_tbl.keys():
                fd_tbl = ver_tbl[ver_pair]
                for fd_pair in fd_tbl.keys():
                    g_tbl = fd_tbl[fd_pair]
                    for g in g_tbl.keys():
                        r_tbl = g_tbl[g]
                        for change_pat in r_tbl.keys():
                            count_tbl[change_pat] = \
                                count_tbl.get(change_pat, 0) + len(r_tbl[change_pat])

        html_head = HTML_HEAD % {'proj_id': self._proj_id,
                                 'url_base_path': url_base_path
                                 }

        f_summary.write(html_head)

        f_summary.write('<h3>Total</h3>\n')

        # cov = self.get_total_coverage()
        cov = None

        html_tbl_data['npatterns'] = format_num(len(count_tbl.keys()))

        self._show_stat(f_summary, count_tbl, cov)

        ver_pair_count = 0
        file_count = 0
        count = 0

        xkey_tbl = {}

        first_path_sub = None

        for lang in self._result.keys():
            try:
                xkey_tbl_lang = xkey_tbl[lang]
            except KeyError:
                xkey_tbl_lang = {}
                xkey_tbl[lang] = xkey_tbl_lang

            ver_tbl = self._result[lang]
            ver_pairs = list(ver_tbl.keys())

            self.setup_metric_tbl(ver_pairs)
            self.sort_ver_pairs(ver_pairs)

            for (ver0, ver1) in ver_pairs:

                # BEGIN EXTRACT FACT
                if self.extract_fact(lang) and self._extra_fact_extractor:
                    sent0 = self.get_srctree_ent(ver0)
                    sent1 = self.get_srctree_ent(ver1)
                    self._extra_fact_extractor.extract_srctree_fact(fact,
                                                                    sent0,
                                                                    sent1,
                                                                    ver0,
                                                                    ver1)
                # END EXTRACT FACT

                lver0 = get_localname(ver0)
                lver1 = get_localname(ver1)

                lvp = f'{lver0}:{lver1}'
                local_count = 0
                try:
                    xkey_tbl_lang_vp = xkey_tbl_lang[lvp]
                except KeyError:
                    xkey_tbl_lang_vp = {}
                    xkey_tbl_lang[lvp] = xkey_tbl_lang_vp

                path_sub = os.path.join(html_dir, '%d.html' % ver_pair_count)

                vpid = str(ver_pair_count)

                ver_pair_count += 1

                if first_path_sub is None:
                    first_path_sub = path_sub

                f_sub = open(path_sub, 'w')
                f_sub.write(html_head)

                f_summary.write('<hr />\n')

                aver0 = self.get_abbrev_ver(lver0)
                aver1 = self.get_abbrev_ver(lver1)

                f_summary.write('<h3>Between "{}" and "{}"</h3>\n'
                                .format(aver0, aver1))

                if self._metric_tbl:
                    m = self._metric_tbl[(ver0, ver1)]
                    copts_str = m['copts']
                    if copts_str:
                        copts_str = ' (%s)' % copts_str
                    met = '<span class="term">{}:</span> {}{}<br />\n'\
                        .format(self._extra_fact_extractor.metrics,
                                m['value'],
                                copts_str)
                    f_summary.write(met)

                other_info = self._extra_fact_extractor.get_other_info(ver0,
                                                                       ver1)
                if other_info:
                    for k in other_info.keys():
                        f_summary.write('<span class="term">{}:</span> {}<br />\n'
                                        .format(k, other_info[k]))

                f_sub.write('<h3>Patterns found between "{}" and "{}"</h3>\n'
                            .format(aver0, aver1))

                fd_tbl = ver_tbl[(ver0, ver1)]

                instances = []

                for fd_pair in fd_tbl.keys():
                    g_tbl = fd_tbl[fd_pair]
                    for g in g_tbl.keys():
                        r_tbl = g_tbl[g]
                        for change_pat in r_tbl.keys():
                            i_tbl = r_tbl[change_pat]
                            for k in i_tbl.keys():
                                (v0, v1, e0, e1) = i_tbl[k]
                                iid = (change_pat, k)
                                instances.append(iid)

                ninstances = len(instances)
                logger.info('{} instances found between "{}" and "{}"'
                            .format(ninstances, lver0, lver1))

                if self._limit is not None:
                    if ninstances > self._limit:
                        reduced_count = 0
                        reduced_instances = random.sample(instances,
                                                          self._limit)
                        logger.info('--> randomly sampled {} instances'
                                    .format(len(reduced_instances)))

                        for fd_pair in fd_tbl.keys():
                            g_tbl = fd_tbl[fd_pair]
                            for g in g_tbl.keys():
                                r_tbl = g_tbl[g]
                                for change_pat in r_tbl.keys():
                                    rs = []
                                    i_tbl = r_tbl[change_pat]
                                    for k in i_tbl.keys():
                                        (v0, v1, e0, e1) = i_tbl[k]
                                        iid = (change_pat, k)
                                        if iid in reduced_instances:
                                            rs.append((v0, v1, e0, e1))
                                            reduced_count += 1
                                    r_tbl[change_pat] = rs

                        logger.info(f'--> {reduced_count} instances selected')

                for fd_pair in fd_tbl.keys():
                    g_tbl = fd_tbl[fd_pair]
                    for g in g_tbl.keys():
                        r_tbl = g_tbl[g]
                        for change_pat in r_tbl.keys():
                            if r_tbl[change_pat] == []:
                                del r_tbl[change_pat]

                for fd_pair in fd_tbl.keys():
                    if fd_tbl[fd_pair] == {}:
                        del fd_tbl[fd_pair]

                count_tbl = {}
                for fd_pair in fd_tbl.keys():
                    g_tbl = fd_tbl[fd_pair]
                    for g in g_tbl.keys():
                        r_tbl = g_tbl[g]
                        for change_pat in r_tbl.keys():
                            count_tbl[change_pat] = \
                                count_tbl.get(change_pat, 0) \
                                + len(r_tbl[change_pat].keys())

                # cov = self.get_coverage_v(ver0, ver1)
                cov = None

                self._show_stat(f_summary, count_tbl, cov)

                link = '<a href="{}" target="right">Show Detail</a>\n'\
                    .format(os.path.basename(path_sub))
                f_summary.write(link)

                for (fid0, fid1) in fd_tbl.keys():

                    path0 = self.get_source_path(fid0, ver0)
                    path1 = self.get_source_path(fid1, ver1)

                    file_count += 1

                    file_pair_node = None
                    # BEGIN EXTRACT FACT
                    if self.extract_fact(lang):
                        # fent0 = self._fent_tbl[fid0]
                        # fent1 = self._fent_tbl[fid1]
                        fent0 = self.get_fent(fid0)
                        fent1 = self.get_fent(fid1)
                        file_pair_node = mkfilepair(fent0, fent1)
                        ps = self.get_predicates(lang)
                        fact.add(file_pair_node, P_TYPE, ps.c_filepair)
                        fact.add(file_pair_node, ps.p_orig_file, fent0)
                        fact.add(file_pair_node, ps.p_modi_file, fent1)
                        # self._extra_fact_extractor.extract_file_fact(fact, fent0, fent1, ver0, ver1)
                    # END EXTRACT FACT

                    url0 = self.get_source_url(fid0, ver0, path=path0)
                    url1 = self.get_source_url(fid1, ver1, path=path1)

                    f_sub.write('<h4>[{}] {} <br/> - {}</h4>\n'
                                .format(file_count, path0, path1))
                    f_sub.write('<ul>\n')

                    applet_data = {
                        'proj_id': self._proj_id,
                        # 'cache': pathname2url(self._cache_base_dir),
                        # 'diff': self.get_diff_url(fid0, fid1),
                        'algo': self._hash_algo,
                        'ver0': aver0,
                        'ver1': aver1,
                        'path0': path0,
                        'path1': path1,
                        'src0': url0,
                        'src1': url1,
                        'desc': '{}',
                        'ess0': '[]',
                        'ess1': '[]',
                        'url_base_path': url_base_path,
                    }
                    item_data = {}

                    g_tbl = fd_tbl.get((fid0, fid1), {})

                    for g in g_tbl.keys():

                        r_tbl = g_tbl[g]

                        for change_pat in r_tbl.keys():

                            item_data['rname'] = change_pat

                            i_tbl = r_tbl[change_pat]

                            for k in i_tbl.keys():

                                (var0, var1, ent0, ent1) = i_tbl[k]

                                others_tbl = self.get_others(change_pat,
                                                             ent0, ent1)

                                count += 1
                                local_count += 1

                                ln0 = get_localname(str(ent0.get_uri()))
                                ln1 = get_localname(str(ent1.get_uri()))
                                xkey = '%s:%s:%s' % (change_pat, ln0, ln1)
                                xkey_tbl_lang_vp[xkey] = local_count

                                # BEGIN EXTRACT FACT
                                chgpat_node = None
                                if file_pair_node:
                                    chgpat_node = mkchgpat(ent0, ent1,
                                                           change_pat)
                                    chg_ty_node = self.mkchgtype(lang,
                                                                 change_pat)
                                    ps = self.get_predicates(lang)
                                    fact.add(file_pair_node, ps.p_chgpat,
                                             chgpat_node)
                                    fact.add(chgpat_node, ps.p_filepair,
                                             file_pair_node)
                                    fact.add(chgpat_node, P_TYPE, chg_ty_node)
                                    fact.add(chgpat_node,
                                             self.mkchgpatpred(lang, var0),
                                             ent0)
                                    fact.add(chgpat_node,
                                             self.mkchgpatpred(lang, var1),
                                             ent1)
                                # END EXTRACT FACT

                                item_data['count'] = count

                                r0 = ent0.get_range()
                                r1 = ent1.get_range()

                                line0 = r0.get_start_line()
                                line1 = r1.get_start_line()

                                col0 = r0.get_start_col()
                                col1 = r1.get_start_col()

                                applet_data['vpid'] = vpid
                                applet_data['form_id'] = str(count)

                                applet_data['startl0'] = line0
                                applet_data['startl1'] = line1

                                applet_data['startc0'] = col0
                                applet_data['startc1'] = col1

                                applet_data['hilit0'] = self.get_hilit(r0)
                                applet_data['hilit1'] = self.get_hilit(r1)

                                applet_data['var0'] = var0
                                applet_data['var1'] = var1

                                otbl_keys = sorted(list(others_tbl.keys()))

                                # BEGIN EXTRACT FACT
                                if chgpat_node:
                                    for k in otbl_keys:
                                        objs = []
                                        for s in others_tbl[k]:
                                            if s.startswith('http'):
                                                objs.append(Resource(uri=s))
                                            else:
                                                objs.append(make_literal(s))

                                        if len(objs) > 0:
                                            pred = self.mkchgpatpred(lang, k)

                                        for obj in objs:
                                            fact.add(chgpat_node, pred, obj)

                                    # for (pred, lits) in [(self.mkchgpatpred(lang, k), make_literals(others_tbl[k])) for k in otbl_keys]:
                                    #     for lit in lits:
                                    #         fact.add(chgpat_node, pred, lit)
                                # END EXTRACT FACT

                                other_vs = []
                                for k in otbl_keys:
                                    ss = list(filter(lambda x: not x.startswith('http'), others_tbl[k]))
                                    if len(ss) > 0:
                                        other_vs.append('<span class="variable">%s</span>: <code>%s</code>' % (k, str_set_to_str(ss)))
                                # other_vs = ['<span class="variable">%s</span>: <code>%s</code>' % (k, str_set_to_str(others_tbl[k])) for k in otbl_keys]
                                others_str = ', '.join(other_vs)
                                item_data['others'] = others_str

                                #

                                desc = others_tbl.copy()
                                for (k, v) in desc.items():
                                    if isinstance(v, set):
                                        if len(v) == 1:
                                            desc[k] = v.pop()
                                        else:
                                            desc[k] = list(v)

                                desc['$cid'] = count
                                desc['$change'] = change_pat
                                desc_str = jsondumps(desc)

                                #

                                ess0 = self.get_essentials_RM(change_pat,
                                                              ent0, ent1)
                                ess1 = self.get_essentials_AD(change_pat,
                                                              ent0, ent1)
                                ess01 = self.get_essentials_MP(change_pat,
                                                               ent0, ent1)

                                ess_tbl0 = {}  # fid -> (so * eo * var) list
                                ess_tbl1 = {}  # fid -> (so * eo * var) list
                                ess_tbl01 = {}  # (fid * fid) -> ((so * eo * var) * (so * eo * var)) list

                                def mkofs(e, v):
                                    r = e.get_range()
                                    so = r.get_start_offset()
                                    eo = r.get_end_offset()
                                    ofs = None
                                    if so == eo:
                                        ofs = (so, v)
                                    elif so < eo:
                                        ofs = (so, eo, v)
                                    return ofs

                                def mk_ess_tbl(ess, ess_tbl):
                                    for (v, es) in ess.items():
                                        m = isinstance(v, tuple)
                                        for e in es:
                                            ofs = None
                                            if m:
                                                ofs = (mkofs(e[0], v[0]),
                                                       mkofs(e[1], v[1]))
                                            else:
                                                ofs = mkofs(e, v)

                                            if ofs:
                                                ofss = []
                                                if m:
                                                    fd = (e[0].get_file_id(),
                                                          e[1].get_file_id())
                                                else:
                                                    fd = e.get_file_id()
                                                try:
                                                    ofss = ess_tbl[fd]
                                                except KeyError:
                                                    ess_tbl[fd] = ofss

                                                ofss.append(ofs)

                                mk_ess_tbl(ess0, ess_tbl0)
                                mk_ess_tbl(ess1, ess_tbl1)
                                mk_ess_tbl(ess01, ess_tbl01)

                                def finish_ess_tbl(tbl):
                                    for (fd, ofss) in tbl.items():
                                        tbl[fd] = jsondumps(sorted(ofss))

                                finish_ess_tbl(ess_tbl0)
                                finish_ess_tbl(ess_tbl1)
                                finish_ess_tbl(ess_tbl01)

                                fid0 = ent0.get_file_id()
                                fid1 = ent1.get_file_id()

                                applet_data['desc'] = desc_str
                                applet_data['ess0'] = ess_tbl0.get(fid0, '[]')
                                applet_data['ess1'] = ess_tbl1.get(fid1, '[]')
                                applet_data['ess01'] = ess_tbl01.get((fid0, fid1), '[]')

                                #

                                applet = APPLET % applet_data

                                extra_pairs = self.get_extra_ent_pairs(change_pat,
                                                                       ent0, ent1)

                                aps = []

                                sub_count = 0

                                for (v0, v1, e0, e1) in extra_pairs:
                                    f0 = e0.get_file_id()
                                    f1 = e1.get_file_id()
                                    ess0_str = ess_tbl0.get(f0, '[]')
                                    ess1_str = ess_tbl1.get(f1, '[]')
                                    ess01_str = ess_tbl01.get((f0, f1), '[]')

                                    form_id = '%d-%d' % (count, sub_count)

                                    a = self.make_applet_html(vpid, form_id,
                                                              v0, v1, e0, e1,
                                                              ver0, ver1,
                                                              desc_str,
                                                              ess0_str,
                                                              ess1_str,
                                                              ess01_str,
                                                              url_base_path)

                                    if a:
                                        aps.append(a)
                                        sub_count += 1

                                extra_applets = ''.join(aps)

                                # BEGIN EXTRACT FACT
                                if chgpat_node:
                                    for (v0, v1, e0, e1) in extra_pairs:
                                        fact.add(chgpat_node,
                                                 self.mkchgpatpred(lang, v0),
                                                 e0)
                                        fact.add(chgpat_node,
                                                 self.mkchgpatpred(lang, v1),
                                                 e1)
                                # END EXTRACT FACT

                                item_data['applet'] = applet + extra_applets

                                f_sub.write(HTML_ITEM % item_data)

                    f_sub.write('</ul>\n')

                    # dump unidentified changes

                    count_uc = 0

                    chgs = []

                    try:
                        chgs = self._prim_chg_tbl[(ver0, ver1)][(fid0, fid1)]
                    except KeyError:
                        pass

                    reduced_chgs = self.reduce_chgs(chgs)

                    if reduced_chgs:
                        f_sub.write('<h5>Other Changes</h5>\n')

                    for (e0, e1, c, ct) in reduced_chgs:

                        r0 = e0.get_range()
                        r1 = e1.get_range()

                        ln0 = r0.get_start_line()
                        ln1 = r1.get_start_line()

                        col0 = r0.get_start_col()
                        col1 = r1.get_start_col()

                        applet_data['startl0'] = ln0
                        applet_data['startl1'] = ln1

                        applet_data['startc0'] = col0
                        applet_data['startc1'] = col1

                        applet_data['hilit0'] = self.get_hilit(r0)
                        applet_data['hilit1'] = self.get_hilit(r1)

                        applet_data['var0'] = '_'
                        applet_data['var1'] = '_'

                        applet_data['form_id'] = 'o%d' % count_uc

                        applet = APPLET % applet_data

                        count_uc += 1

                        item_data['rname'] = '%s (%s)' % (c, ct)
                        item_data['count'] = count_uc
                        item_data['others'] = ''
                        item_data['applet'] = applet

                        f_sub.write('<ul>\n')
                        f_sub.write(HTML_ITEM % item_data)
                        f_sub.write('</ul>\n')

                f_sub.write(HTML_TAIL)
                f_sub.close()

        f_summary.write(HTML_TAIL)
        f_summary.close()

        f = open(html_path, 'w')

        summary = ''
        if path_summary:
            summary = os.path.basename(path_summary)
        first = ''
        if first_path_sub:
            first = os.path.basename(first_path_sub)

            f.write(FRAME_PAGE % {'proj_id': self._proj_id,
                                  'summary': summary,
                                  'first': first,
                                  'year': datetime.today().year,
                                  'url_base_path': url_base_path,
                                  })

        f.close()

        if self.extract_fact(lang):
            fdir = outdir
            if foutdir:
                fdir = foutdir
            fact.write(os.path.join(fdir, self._proj_id+'.ttl'))

        # html table for index.html
        path_html_tbl = os.path.join(outdir, self._proj_id+'.tdata')
        f_html_tbl = open(path_html_tbl, 'w')
        html_tbl_data['ntriples'] = format_num(self.count_triples())

        html_tbl_data0 = html_tbl_data.copy()
        html_tbl_data0['html'] = os.path.join(self._proj_id, 'index.html')

        f_html_tbl.write(HTML_TBL % html_tbl_data0)
        f_html_tbl.close()
        ##

        with open(os.path.join(outdir, self._proj_id+'.json'), 'w') as f:
            tbl = {self._proj_id: xkey_tbl}
            json.dump(tbl, f)

    def get_vindex(self, uri):
        vi = None
        try:
            vi = self._conf.versionURIs.index(uri)
        except Exception:
            pass
        return vi

    def get_ent(self, uri):
        try:
            ent = SourceCodeEntity(uri=uri)
            return ent
        except Exception as e:
            logger.error(f'{e}: uri=\"{uri}\"')

    def get_uri(self, row, var):
        uri = row[var]
        if not uri:
            logger.warning(f'cannot get URI for "{var}"')
        return uri

    def add_extra_ent_pair(self, chgpat, bent0_bent1, var0_var1, ent0_ent1):
        bent0, bent1 = bent0_bent1
        var0, var1 = var0_var1
        ent0, ent1 = ent0_ent1
        vtbl = {}
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            vtbl = self._extra_ent_pairs_tbl[bkey]
        except KeyError:
            self._extra_ent_pairs_tbl[bkey] = vtbl

        eps = set()
        try:
            eps = vtbl[(var0, var1)]
        except KeyError:
            vtbl[(var0, var1)] = eps

        eps.add((ent0, ent1))

    def get_extra_ent_pairs(self, chgpat, bent0, bent1):
        res = []
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            vtbl = self._extra_ent_pairs_tbl[bkey]
            for (v0, v1) in vtbl.keys():
                for (e0, e1) in vtbl[(v0, v1)]:
                    res.append((v0, v1, e0, e1))
        except KeyError:
            pass

        res.sort(key=lambda x: factutil.range.Key(x[2].get_range()))

        return res

    def add_other(self, chgpat, bent0_bent1, key, val):
        bent0, bent1 = bent0_bent1
        vtbl = {}
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            vtbl = self._others_tbl[bkey]
        except KeyError:
            self._others_tbl[bkey] = vtbl

        vs = set()
        try:
            vs = vtbl[key]
        except KeyError:
            vtbl[key] = vs

        vs.add(val)

    def get_others(self, chgpat, bent0, bent1):
        vtbl = {}
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            vtbl = self._others_tbl[bkey]
        except KeyError:
            pass

        return vtbl

    def add_essential(self, ess_tbl, chgpat, bent0_bent1, var, val):
        bent0, bent1 = bent0_bent1
        vtbl = {}
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            vtbl = ess_tbl[bkey]
        except KeyError:
            ess_tbl[bkey] = vtbl

        s = set()
        try:
            s = vtbl[var]
        except KeyError:
            vtbl[var] = s

        s.add(val)

    def add_essential_RM(self, chgpat, bent01, var, val):
        self.add_essential(self._ess_tbl_rm, chgpat, bent01, var, val)

    def add_essential_AD(self, chgpat, bent01, var, val):
        self.add_essential(self._ess_tbl_ad, chgpat, bent01, var, val)

    def add_essential_MP(self, chgpat, bent01, var, val):
        self.add_essential(self._ess_tbl_mp, chgpat, bent01, var, val)

    def get_essentials(self, ess_tbl, chgpat, bent0, bent1):
        tbl = {}
        bkey = (chgpat, bent0.get_uri(), bent1.get_uri())
        try:
            tbl = ess_tbl[bkey]
        except KeyError:
            pass
        return tbl

    def get_essentials_RM(self, chgpat, bent0, bent1):
        return self.get_essentials(self._ess_tbl_rm, chgpat, bent0, bent1)

    def get_essentials_AD(self, chgpat, bent0, bent1):
        return self.get_essentials(self._ess_tbl_ad, chgpat, bent0, bent1)

    def get_essentials_MP(self, chgpat, bent0, bent1):
        return self.get_essentials(self._ess_tbl_mp, chgpat, bent0, bent1)

    def _enumerate_changes(self, lang, q):

        logger.info('enumerating changes (%s)...' % q),

        start = time.time()

        try:
            _query, per_ver = self.get_query(lang, q)

            tbl = {}

            if per_ver:
                for (ver0, ver1) in self._conf.vURIpairs:
                    logger.info('  "{}"-"{}"'
                                .format(get_localname(ver0),
                                        get_localname(ver1)))

                    count = 0

                    query = self.add_ver_filter(_query, ver0, ver1)

                    for qvs, row in self._sparql.query(query):
                        uri0 = self.get_uri(row, 'ent')
                        uri1 = self.get_uri(row, 'ent_')
                        chg = self.get_uri(row, 'chg')
                        cat = get_localname(self.get_uri(row, 'cat'))

                        skip = chg == 'modified'

                        if uri0 is None or uri1 is None or skip:
                            continue

                        ent0 = self.get_ent(uri0)
                        ent1 = self.get_ent(uri1)

                        if ent0.is_file() and ent1.is_file():
                            continue

                        try:
                            tbl[(ver0, ver1)].append((ent0, ent1, chg, cat))
                        except KeyError:
                            tbl[(ver0, ver1)] = [(ent0, ent1, chg, cat)]

                        count += 1

                    logger.debug('%d rows processed' % count)

            else:
                count = 0

                for qvs, row in self._sparql.query(_query):
                    ver0 = self.get_uri(row, 'ver')
                    ver1 = self.get_uri(row, 'ver_')
                    uri0 = self.get_uri(row, 'ent')
                    uri1 = self.get_uri(row, 'ent_')
                    chg = self.get_uri(row, 'chg')
                    cat = get_localname(self.get_uri(row, 'cat'))

                    skip = chg == 'modified'

                    if uri0 is None or uri1 is None or skip:
                        continue

                    ent0 = self.get_ent(uri0)
                    ent1 = self.get_ent(uri1)

                    if ent0.is_file() and ent1.is_file():
                        continue

                    try:
                        tbl[(ver0, ver1)].append((ent0, ent1, chg, cat))
                    except KeyError:
                        tbl[(ver0, ver1)] = [(ent0, ent1, chg, cat)]

                    count += 1

                logger.debug('%d rows processed' % count)

            for (ver0, ver1) in tbl.keys():
                fd_tbl = {}
                try:
                    fd_tbl = self._prim_chg_tbl[(ver0, ver1)]
                except KeyError:
                    self._prim_chg_tbl[(ver0, ver1)] = fd_tbl

                for (ent0, ent1, chg, cat) in tbl[(ver0, ver1)]:

                    fid0 = ent0.get_file_id()
                    fid1 = ent1.get_file_id()

                    self.add_fent(fid0, SourceCodeEntity(file_id=fid0))
                    self.add_fent(fid1, SourceCodeEntity(file_id=fid1))

                    chgs = set()
                    try:
                        chgs = fd_tbl[(fid0, fid1)]
                    except KeyError:
                        fd_tbl[(fid0, fid1)] = chgs

                    chgs.add((ent0, ent1, chg, cat))

            logger.debug('primitive changes:')

            self._prim_chg_count_tbl = {}

            for k0 in self._prim_chg_tbl.keys():

                (ver0, ver1) = k0

                logger.debug('  {}-{}:'
                             .format(get_localname(ver0),
                                     get_localname(ver1)))

                fd_tbl = {}
                try:
                    fd_tbl = self._prim_chg_count_tbl[k0]
                except KeyError:
                    self._prim_chg_count_tbl[k0] = fd_tbl

                for k1 in self._prim_chg_tbl[k0].keys():
                    (fid0, fid1) = k1
                    n = len(self._prim_chg_tbl[k0][k1])

                    logger.debug('    "{}"-"{}": {}'
                                 .format(fid0.encode(), fid1.encode(), n))

                    self._prim_chg_count_tbl[k0][k1] = n

            t = time.time() - start
            logger.info('done. (%ds)' % t)

        except QueryNotFound as e:
            logger.warning('query not found: "%s"' % e.query)

    def enumerate_primitive_changes(self, lang):
        logger.info('enumerating primitive changes for %s...' % lang)
        self._prim_chg_tbl = {}
        lang = None
        self._enumerate_changes(lang, Q_ENUM_MAPPING_CHG)
        self._enumerate_changes(lang, Q_ENUM_ADDITION)
        self._enumerate_changes(lang, Q_ENUM_REMOVAL)

    def add_fent(self, fid, e):
        if fid not in self._fent_tbl:
            self._fent_tbl[fid] = e

    def get_fent(self, fid):
        fent = None
        try:
            fent = self._fent_tbl[fid]
        except KeyError:
            fent = SourceCodeEntity(file_id=fid)
            self.add_fent(fid, fent)
        return fent

    # def enumerate_files(self):

    #     logger.info('enumerating files...')

    #     query, per_ver = self.get_query(None, Q_ENUM_FILE)

    #     for qvs, row in self._sparql.query(query):
    #         uri = self.get_uri(row, 'ent')

    #         if uri == None:
    #             continue

    #         ent = self.get_ent(uri)

    #         if ent.is_file():
    #             fid = ent.get_file_id()
    #             self.add_fent(fid, SourceCodeEntity(file_id=fid))

    #     logger.info('done.')

    def is_removal(self, s):
        b = s in ('deleted or pruned', 'deleted from', 'pruned from',
                  'weak removal')
        logger.debug('%s --> %s' % (s, b))
        return b

    def is_addition(self, s):
        b = s in ('inserted or grafted', 'inserted into', 'grafted onto',
                  'weak addition')
        logger.debug('%s --> %s' % (s, b))
        return b

    def is_mapping_chg(self, s):
        b = s in ('changed to', 'modified', 'moved to', 'order changed',
                  'renamed')
        logger.debug('%s --> %s' % (s, b))
        return b

    def remove_from_prim_chg_tbl(self, ver0, ver1, fid0, fid1, ent0, ent1):
        try:
            chgs = self._prim_chg_tbl[(ver0, ver1)][(fid0, fid1)]
            to_be_removed = []
            for chg in chgs:
                (e0, e1, c, ct) = chg

                cond = False

                if ent0 and not ent1:
                    cond = ent0.contains(e0) and self.is_removal(c)

                elif not ent0 and ent1:
                    cond = ent1.contains(e1) and self.is_addition(c)

                elif ent0 and ent1:
                    cond0 = ent0.contains(e0) and ent1.contains(e1) \
                        and self.is_mapping_chg(c)
                    cond1 = ent0.contains(e0) and self.is_removal(c)
                    cond2 = ent1.contains(e1) and self.is_addition(c)
                    cond = cond0 or cond1 or cond2

                if cond:
                    to_be_removed.append(chg)

            for chg in to_be_removed:
                logger.debug('removing (%s,%s,%s)' % (e0, e1, c))
                chgs.remove(chg)

        except KeyError:
            pass

    def find(self, force_per_ver=False, query_prec=False):
        self._extra_ent_pair_tbl = {}
        self._result = {}

        if query_prec:
            # self._inst_key_tbl = {} # (ver * ver) -> inst_key set
            self._inst_elemsl_tbl = {}  # (ver * ver) -> elem_set list

        # self.enumerate_files()

        for lang in self._queries.keys():
            if self._lang is not None and lang != self._lang:
                continue

            if self._change_enumeration:
                self.enumerate_primitive_changes(lang)

            try:
                ver_tbl = self._result[lang]  # (ver * ver) -> (fid * fid) -> change_pat_name ->  (ent * ent) list
            except KeyError:
                ver_tbl = {}
                self._result[lang] = ver_tbl

            qdata = self._queries[lang]

            for (q, var0, var1, extra, others, essential,
                 inst_key, inst_key_is_one_to_one,
                 is_complex, min_extra) in qdata:

                if query_prec:
                    skip_count = 0

                name = fname_to_title(q)
                logger.info('finding \"%s\" for %s...' % (name, lang)),
                sys.stdout.flush()

                ess0, ess1, ess01 = essential

                has_extra = len(extra) > 0

                _query, per_ver = self.get_query(lang, q,
                                                 force_per_ver=(force_per_ver
                                                                and is_complex)
                                                 )

                # logger.debug('query:\n%s' % _query)

                start = time.time()

                vpairs = []

                if self._conf.vpairs:
                    if per_ver:
                        vpairs = self._conf.vURIpairs
                    else:
                        vpairs.append(self._conf.vURIpairs[0])
                else:
                    if per_ver:
                        for i in range(self._conf.nversions - 1):
                            v0 = self._conf.versionURIs[i]
                            v1 = self._conf.versionURIs[i+1]
                            vpairs.append((v0, v1))
                    else:
                        vpairs.append((self._conf.versionURIs[0],
                                       self._conf.versionURIs[1]))

                nvpairs = len(vpairs)
                vp_count = 0

                if per_ver:
                    logger.info('')

                for (ver0, ver1) in vpairs:
                    vp_count += 1

                    logger.debug('%s vs %s' % (ver0, ver1))

                    st0 = time.time()

                    if per_ver:
                        query = self.add_ver_filter(_query, ver0, ver1)
                        sys.stdout.write('<{} -> {}> [{}/{}]'
                                         .format(self.get_ver_name(ver0),
                                                 self.get_ver_name(ver1),
                                                 vp_count,
                                                 nvpairs))
                        sys.stdout.flush()
                    else:
                        query = _query

                    # logger.debug('query:\n%s' % query)

                    rows = []

                    for qvs, row in self._sparql.query(query):
                        rows.append(row)

                    extra_count_tbl = {}

                    for row in rows:
                        uri0 = self.get_uri(row, var0)
                        uri1 = self.get_uri(row, var1)

                        if uri0 is None or uri1 is None:
                            continue

                        ########################################
                        if query_prec and inst_key:
                            def get(v):
                                u = row.get(v, None)
                                # e = None
                                # if u:
                                #     e = self.get_ent(u)
                                return u

                            ikey = tuple([get(v) for v in inst_key])

                            vp = (ver0, ver1)

                            elemsl = self._inst_elemsl_tbl.get(vp, [])
                            if elemsl == []:
                                self._inst_elemsl_tbl[vp] = [set([x])
                                                             for x in ikey]
                            else:
                                all_match_mode = False
                                any_match_mode = False
                                if inst_key_is_one_to_one:
                                    all_match_mode = not any(inst_key_is_one_to_one)
                                    any_match_mode = all(inst_key_is_one_to_one)

                                def get_elems(i):
                                    try:
                                        elems = elemsl[i]
                                    except IndexError:
                                        elems = set()
                                        elemsl.append(elems)
                                    return elems

                                if any_match_mode:
                                    skip_flag = any([x in get_elems(i)
                                                     for (i, x) in enumerate(ikey)])
                                elif all_match_mode:
                                    skip_flag = all([x in get_elems(i)
                                                     for (i, x) in enumerate(ikey)])
                                else:
                                    vec = []
                                    for (i, x) in enumerate(ikey):
                                        is_one_to_one = False
                                        if inst_key_is_one_to_one:
                                            try:
                                                is_one_to_one = inst_key_is_one_to_one[i]
                                            except IndexError:
                                                pass
                                        if not is_one_to_one:
                                            vec.append(x in get_elems(i))
                                    skip_flag = all(vec)

                                if skip_flag:
                                    skip_count += 1
                                    continue
                                else:
                                    for (i, x) in enumerate(ikey):
                                        get_elems(i).add(x)

                            # if ikey in self._inst_key_tbl.get(vp, []):
                            #     skip_count += 1
                            #     continue
                            # else:
                            #     try:
                            #         s = self._inst_key_tbl[vp]
                            #     except KeyError:
                            #         s = set()
                            #         self._inst_key_tbl[vp] = s
                            #     s.add(ikey)
                        ########################################

                        k = (uri0, uri1)

                        for (v0, v1) in extra:
                            u0 = row[v0]
                            u1 = row[v1]

                            if u0 and u1:
                                e0 = self.get_ent(u0)
                                e1 = self.get_ent(u1)

                                if not self.check_path(e0, e1, ver0, ver1, per_ver):
                                    continue

                                try:
                                    extra_count_tbl[k] += 1
                                except KeyError:
                                    extra_count_tbl[k] = 1

                    for row in rows:

                        uri0 = self.get_uri(row, var0)
                        uri1 = self.get_uri(row, var1)

                        logger.debug('var0: %s --> uri0: %s' % (var0, uri0))
                        logger.debug('var1: %s --> uri1: %s' % (var1, uri1))
                        # print('var0: %s --> %s' % (var0, uri0))
                        # print('var1: %s --> %s' % (var1, uri1))

                        if uri0 is None or uri1 is None:
                            continue

                        if has_extra:
                            extra_count = extra_count_tbl.get((uri0, uri1), 0)
                            logger.debug('extra_count={} min_extra={}'
                                         .format(extra_count, min_extra))
                            if extra_count < min_extra:
                                continue

                        ent0 = self.get_ent(uri0)
                        ent1 = self.get_ent(uri1)

                        fid0 = ent0.get_file_id()
                        fid1 = ent1.get_file_id()

                        if fid0 == fid1:
                            logger.warning('illegal URI pair: {} and {}'
                                           .format(uri0, uri1))

                        version_pairs = [(ver0, ver1)]

                        if not per_ver:
                            version_pairs = self.get_version_pairs(ent0, ent1)
                            if version_pairs == []:
                                logger.warning('%s' % name)
                                continue

                        if self._conf.include:
                            ok = False
                            for (ve0, ve1) in version_pairs:
                                path0 = self.get_source_path(fid0, ve0)
                                path1 = self.get_source_path(fid1, ve1)
                                if path0 or path1:
                                    ok = True
                                    break

                            if not ok:
                                continue

                        key_var = ''
                        try:
                            key_var = row[KEY_VAR_NAME]
                        except Exception:
                            pass

                        chg_name = name
                        if key_var:
                            chg_name = '"%s" %s' % (key_var, name)

                        for (v0, v1) in extra:
                            u0 = row[v0]
                            u1 = row[v1]
                            if u0 and u1:
                                e0 = self.get_ent(u0)
                                e1 = self.get_ent(u1)

                                if not self.check_path(e0, e1, ver0, ver1, per_ver):
                                    continue

                                self.add_extra_ent_pair(chg_name,
                                                        (ent0, ent1),
                                                        (v0, v1), (e0, e1))

                        ess_exists = False

                        for v0 in ess0:
                            u0 = row[v0]
                            if u0:
                                ess_exists = True
                                e0 = self.get_ent(u0)

                                self.add_essential_RM(chg_name, (ent0, ent1), v0, e0)

                                if self._change_enumeration:
                                    for (ver0_, ver1_) in version_pairs:
                                        self.remove_from_prim_chg_tbl(ver0_,
                                                                      ver1_,
                                                                      fid0,
                                                                      fid1,
                                                                      e0,
                                                                      None)

                        for v1 in ess1:
                            u1 = row[v1]
                            if u1:
                                ess_exists = True
                                e1 = self.get_ent(u1)

                                self.add_essential_AD(chg_name,
                                                      (ent0, ent1), v1, e1)

                                if self._change_enumeration:
                                    for (ver0_, ver1_) in version_pairs:
                                        self.remove_from_prim_chg_tbl(ver0_,
                                                                      ver1_,
                                                                      fid0,
                                                                      fid1,
                                                                      None,
                                                                      e1)

                        for (v0, v1) in ess01:
                            u0 = row[v0]
                            u1 = row[v1]
                            if u0 and u1:
                                ess_exists = True
                                e0 = self.get_ent(u0)
                                e1 = self.get_ent(u1)

                                self.add_essential_MP(chg_name,
                                                      (ent0, ent1),
                                                      (v0, v1),
                                                      (e0, e1))

                                if self._change_enumeration:
                                    for (ver0_, ver1_) in version_pairs:
                                        self.remove_from_prim_chg_tbl(ver0_,
                                                                      ver1_,
                                                                      fid0,
                                                                      fid1,
                                                                      e0,
                                                                      e1)

                        if not ess_exists:
                            # logger.warning('no essential variables found')
                            continue

                        for v in others:
                            try:
                                x = row[v]
                                if x:
                                    self.add_other(chg_name, (ent0, ent1), v, x)

                            except Exception as e:
                                logger.warning(str(e))

                        data = (var0, var1, ent0, ent1)

                        key = (ent0.get_uri(), ent1.get_uri())

                        fd_tbls = []
                        for (ver0_, ver1_) in version_pairs:
                            try:
                                fd_tbls.append(ver_tbl[(ver0_, ver1_)])
                            except KeyError:
                                fd_tbl = {}
                                ver_tbl[(ver0_, ver1_)] = fd_tbl
                                fd_tbls.append(fd_tbl)

                        grp = GROUP_SEPARATOR
                        try:
                            gstr = row[GROUP_VAR_NAME]
                            if gstr:
                                gs = [x for x in gstr.split(GROUP_SEPARATOR) if x != '']

                                ys = []
                                for ns in gs:
                                    xs = [x for x in ns.split(NAME_SEPARATOR) if x != '']
                                    xs.sort()
                                    ys.append(NAME_SEPARATOR.join(xs))
                                grp = GROUP_SEPARATOR.join(ys)

                        except Exception as e:
                            logger.warning('group not set: %s' % e)

                        for fd_tbl in fd_tbls:

                            g_tbl = {}

                            try:
                                g_tbl = fd_tbl[(fid0, fid1)]
                            except KeyError:
                                g_tbl = {}
                                fd_tbl[(fid0, fid1)] = g_tbl

                            try:
                                d = g_tbl[grp]

                                try:
                                    t = d[chg_name]
                                    t[key] = data
                                except KeyError:
                                    d[chg_name] = {key: data}

                            except KeyError:
                                d = {chg_name: {key: data}}
                                g_tbl[grp] = d

                    if per_ver:
                        logger.info(' (%ds)' % (time.time() - st0))

                t = time.time() - start
                logger.info('done. (%ds)' % t)

                if query_prec and skip_count:
                    logger.info('%d instances skipped' % skip_count)


def find(query_dir, queries, predicate_tbl, extra_fact_extractor,
         base_dir, proj_id, foutdir, outdir, pw, port,
         limit, lang, method, change_enumeration, per_ver, query_prec,
         conf=None, url_base_path='..'):

    finder = Finder(query_dir, queries, base_dir, proj_id,
                    predicate_tbl=predicate_tbl, limit=limit, lang=lang,
                    extra_fact_extractor=extra_fact_extractor, conf=conf,
                    method=method, pw=pw, port=port)

    if change_enumeration:
        finder.enable_change_enumeration()

    finder.find(force_per_ver=per_ver, query_prec=query_prec)

    fact_outdir = foutdir
    if fact_outdir == DEFAULT_FACT_OUTPUT_DIR:
        fact_outdir = fact_outdir.replace('<PROJ_ID>', proj_id)

    finder.dump(outdir, fact_outdir, url_base_path=url_base_path)


def main(query_dir, queries, desc, predicate_tbl=None,
         extra_fact_extractor=None):

    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('work_dir', type=str,
                        help="diffts's work directory for factbase construction")

    parser.add_argument('proj_id', type=str, help='project id')

    parser.add_argument('--port', dest='port', default=VIRTUOSO_PORT,
                        metavar='PORT', type=int, help='set port number')

    parser.add_argument('--pw', dest='pw', metavar='PASSWORD',
                        default=VIRTUOSO_PW,
                        help='set password to access DB')

    parser.add_argument('-m', '--method', dest='method', default='odbc',
                        metavar='METHOD', type=str,
                        help='execute query via METHOD (http|odbc)')

    parser.add_argument('-d', '--debug', dest='debug', action='store_true',
                        help='enable debug printing')

    parser.add_argument('-c', '--enable-change-enumeration',
                        dest='change_enumeration',
                        action='store_true',
                        help='enable change enumeration')

    parser.add_argument('-o', '--outdir', dest='outdir', default='.',
                        metavar='DIR', type=str, help='dump result into DIR')

    parser.add_argument('-f', '--fact-outdir', dest='foutdir',
                        default=DEFAULT_FACT_OUTPUT_DIR,
                        metavar='DIR', type=str, help='dump fact into DIR')

    parser.add_argument('-l', '--limit', dest='limit', default=None,
                        metavar='N', type=int,
                        help='at most N instances are reported for each version pair')

    parser.add_argument('--force-per-ver', dest='per_ver', action='store_true',
                        help='force complex queries to be executed per version pairs')

    parser.add_argument('--query-prec', dest='query_prec', action='store_true',
                        help='recognize query precedence')

    parser.add_argument('--lang', dest='lang', default=None,
                        metavar='LANG', type=str,
                        help='query only for LANG (c|java|fortran)')

    args = parser.parse_args()

    log_level = logging.INFO
    if args.debug:
        log_level = logging.DEBUG
    setup_logger(logger, log_level)

    find(query_dir, queries, predicate_tbl, extra_fact_extractor,
         args.work_dir, args.proj_id, args.foutdir, args.outdir, args.pw, args.port,
         args.limit, args.lang, args.method,
         args.change_enumeration, args.per_ver, args.query_prec)
