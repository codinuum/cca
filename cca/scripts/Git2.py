#!/usr/bin/env python3

'''
  A pygit2 wrapper

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

# Fortran extensions added by Masatomo Hashimoto <m.hashimoto@riken.jp>

import os
import pygit2
from datetime import datetime, timedelta, tzinfo
import re

import pathsetup
import dp
from factutils.fileid import FileDigest, HashAlgo



###############

EXTS = ['.c', '.h', '.py', '.java', '.v',
        '.f', '.for', '.ftn', '.f90', '.f95', '.f03', '.f08']

def shorten_sha(sha):
    return sha[0:7]

def get_fid(blob):
    fid = FileDigest(HashAlgo.GIT, stream=blob.data_stream.stream)
    return fid

def issrc(name):
    b = False
    if name:
        for ext in EXTS:
            if name.endswith(ext):
                b = True
                break
            else:
                cap = ext.upper()
                if name.endswith(cap):
                    b = True
                    break
    return b

class InvalidReference(Exception):
    pass

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

def get_date(commit):
    ts = commit.commit_time
    ofs = commit.commit_time_offset
    dt = datetime.fromtimestamp(ts, TZ(ofs))
    return dt

def get_date_jst(commit):
    dt = get_date(commit)
    jst_dt = dt.astimezone(JST())
    return jst_dt

def get_date_jst_str(commit):
    jst_dt = get_date_jst(commit)
    s = jst_dt.isoformat(' ')
    return s

def get_oid(h):
    return pygit2.Oid(hex=h)

def objty_to_str(ty):
    s = 'UNKNOWN'
    if ty == pygit2.GIT_OBJ_COMMIT:
        s = 'COMMIT'
    elif ty == pygit2.GIT_OBJ_TREE:
        s = 'TREE'
    elif ty == pygit2.GIT_OBJ_BLOB:
        s = 'BLOB'
    elif ty == pygit2.GIT_OBJ_TAG:
        s = 'TAG'
    return s


class User(dp.base):
    def __init__(self, u):
        self.name = u.name
        self.time = u.time
        self.offset = u.offset
        self.email = u.email

class Commit(dp.base):
    def __init__(self, c):
        self.oid = c.oid
        self.id = c.id
        self.author = User(c.author)
        self.committer = User(c.committer)
        self.message = c.message
        self.message_encoding = c.message_encoding


class Repository(dp.base):
    def __init__(self, repo_path, repo=None):
        if repo:
            self._repo = repo
        else:
            self._repo = pygit2.Repository(repo_path)

    # def pull(self):
    #     origin = self._repo.remotes['origin']
    #     stats = origin.fetch()
    #     self.message('total objects: %s' % stats.total_objects)
    #     if stats.total_objects > 0:
    #         fetch_head = self._repo.revparse_single('FETCH_HEAD')
    #         self.message('merging...')
    #         self._repo.merge(fetch_head.id)
    #         self.message('done.')

    def pull(self, remote_name='origin'):
        for remote in self._repo.remotes:
            if remote.name == remote_name:
                stats = remote.fetch()
                self.message('total objects: %s' % stats.total_objects)
                remote_master_id = self._repo.lookup_reference('refs/remotes/origin/master').target
                merge_result, _ = self._repo.merge_analysis(remote_master_id)
                # Up to date, do nothing
                if merge_result & pygit2.GIT_MERGE_ANALYSIS_UP_TO_DATE:
                    return
                # We can just fastforward
                elif merge_result & pygit2.GIT_MERGE_ANALYSIS_FASTFORWARD:
                    self._repo.checkout_tree(self._repo.get(remote_master_id))
                    master_ref = self._repo.lookup_reference('refs/heads/master')
                    master_ref.set_target(remote_master_id)
                    self._repo.head.set_target(remote_master_id)
                elif merge_result & pygit2.GIT_MERGE_ANALYSIS_NORMAL:
                    self._repo.merge(remote_master_id)

                    if self._repo.index.conflicts is not None:
                        self.warning('conflicts: %s' % self._repo.index.conflicts)

                    user = self._repo.default_signature
                    tree = self._repo.index.write_tree()
                    commit = self._repo.create_commit('HEAD',
                                                      user,
                                                      user,
                                                      'Merge!',
                                                      tree,
                                                      [self._repo.head.target, remote_master_id])
                    self._repo.state_cleanup()
                else:
                    self.warning('Unknown merge analysis result')

    def blame(self, path, h_, h, line_min, line_max):
        oid = self.get_commit(h).id
        oid_ = self.get_commit(h_).id
        b = self._repo.blame(path, newest_commit=oid_, oldest_commit=oid, min_line=line_min, max_line=line_max)
        return b

    def list_branches(self):
        l = []
        for bn in self._repo.listall_branches():
            br = self._repo.lookup_branch(bn)
            hc = br.get_object()
            l.append(hc)
        return l

    def _get_obj(self, k):
        obj = None
        if isinstance(k, str):
            obj = self._repo.revparse_single(k)
        elif isinstance(k, pygit2.Oid):
            obj = self._repo.revparse_single(str(k))
        return obj

    def has_type(self, k, ty):
        b = False
        obj = self._get_obj(k)
        if obj:
            b = obj.type == ty
        return b

    def get_obj(self, k, ty):
        obj = self._get_obj(k)
        if obj:
            if obj.type != ty:
                #self.debug('%s != %s' % (objty_to_str(obj.type), objty_to_str(ty)))
                raise (InvalidReference(k))
        else:
            raise (InvalidReference(k))
        return obj

    def is_commit(self, x):
        if isinstance(x, str) or isinstance(x, pygit2.Oid):
            return self.has_type(x, pygit2.GIT_OBJ_COMMIT)
        else:
            return isinstance(x, pygit2.Tag)

    def is_tag(self, x):
        if isinstance(x, str) or isinstance(x, pygit2.Oid):
            return self.has_type(x, pygit2.GIT_OBJ_TAG)
        else:
            return isinstance(x, pygit2.Tag)

    def is_tree(self, x):
        if isinstance(x, str) or isinstance(x, pygit2.Oid):
            return self.has_type(x, pygit2.GIT_OBJ_TREE)
        else:
            return isinstance(x, pygit2.Tree)

    def is_blob(self, x):
        if isinstance(x, str) or isinstance(x, pygit2.Oid):
            self.has_type(x, pygit2.GIT_OBJ_BLOB)
        else:
            return isinstance(x, pygit2.Blob)


    def get_commit(self, k):
        c = None
        try:
            c = self.get_obj(k, pygit2.GIT_OBJ_COMMIT)
        except InvalidReference:
            t = self.get_tag(k)
            c = t.get_object()
            if c.type != pygit2.GIT_OBJ_COMMIT:
                raise (InvalidReference(k))
        return c

    def get_commit_info(self, k):
        return Commit(self.get_commit(k))

    def get_tree(self, k):
        t = None
        try:
            t = self.get_obj(k, pygit2.GIT_OBJ_TREE)
        except InvalidReference:
            c = self.get_commit(k)
            t = c.tree
            if t.type != pygit2.GIT_OBJ_TREE:
                raise (InvalidReference(k))
        return t

    def get_blob(self, k):
        return self.get_obj(k, pygit2.GIT_OBJ_BLOB)

    def get_tag(self, k):
        return self.get_obj(k, pygit2.GIT_OBJ_TAG)

    def tree_of_commit(self, ck):
        c = self.get_commit(ck)
        return c.tree

    def _diff_trees(self, tree0, tree1):
        changed = []
        d = tree0.diff_to_tree(tree1)
        for p in d:
            old_file = p.delta.old_file.path
            new_file = p.delta.new_file.path
            if issrc(old_file) or issrc(new_file):
                changed.append((old_file, new_file))

        if changed:
            self.message('%d changed source files found' % len(changed))
        else:
            self.message('no changed source files found')

        return changed

    def _diff_commits(self, commit0, commit1):
        return self._diff_trees(commit0.tree, commit1.tree)

    def get_changed(self, c0k, c1k):
        c0 = self.get_commit(c0k)
        c1 = self.get_commit(c1k)
        return self._diff_commits(c0, c1)

    def walk_range(self, exc, frm):
        oid_frm = self.get_commit(frm)
        oid_exc = self.get_commit(exc)
        w = self._repo.walk(oid_frm.id, pygit2.GIT_SORT_TOPOLOGICAL)
        w.hide(oid_exc.id)
        return w

    def list_from(self, k, n):
        c = self.get_commit(k)
        l = [c]
        count = 1
        while n < 1 or count < n:
            ps = c.parents
            if ps:
                c = ps[0]
                l.insert(0, c)
                count += 1
            else:
                break
        return l

    def list_info_from(self, k, n):
        return [Commit(c) for c in self.list_from(k, n)]

    def simple_range(self, frm, to):
        ini = self.get_commit(frm)
        cur = self.get_commit(to)
        l = [cur]
        while True:
            ps = cur.parents
            if ps:
                cur = ps[0]
                l.insert(0, cur)
                if cur == ini:
                    break
            else:
                break

        return l

    def simple_info_range(self, frm, to):
        return [Commit(c) for c in self.simple_range(frm, to)]

    def acc(self, rev, path):
        c = self.get_commit(rev)
        obj = c.tree
        p = path.split(os.sep)
        for x in p:
            if obj:
                ent = obj[x]
                obj = self._repo.get(ent.id, None)
            else:
                break
        return obj

    def acc_info(self, rev, path):
        return Commit(self.acc(rev, path))

    def diff(self, a, b):
        return self._repo.diff(a, b)


    def create_blob_hex_list(self, tree, filt):
        l = []
        def traverse(t):
            for x in t:
                o = self._get_obj(x.hex)
                if self.is_tree(o):
                    traverse(self.get_tree(x.id))

                elif self.is_blob(o):
                    if filt(x.name):
                        l.append(x.hex)

        traverse(tree)
        return l

    def find_files(self, rev, filt): # {path,hex} list
        l = []
        tree = self.get_tree(rev)
        def traverse(path, t):
            for x in t:
                path_ = os.path.join(path, x.name)
                try:
                    o = self._get_obj(x.hex)
                    if self.is_tree(o):
                        traverse(path_, self.get_tree(x.id))

                    elif self.is_blob(o):
                        if filt(x.name):
                            l.append({'path':path_,'id':x.hex})

                except Exception as e:
                    self.warning(str(e))
                    continue

        traverse('', tree)
        return l
        

def clone_repository(url, path, bare=False, checkout_branch=None):
    repo = pygit2.clone_repository(url, path, bare=bare, checkout_branch=checkout_branch)
    return Repository('', repo=repo)
        


def test(repo_path):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)
    for c in r.list_branches():
        print(' COMMIT:%s by %s' % (c.id, c.author.name))
        for p in c.parents:
            print('  PARENT:%s by %s' % (p.id, p.author.name))

def test2(repo_path, r0, r1):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)
    c = r.get_changed(r0, r1)
    print('%d changed source files found between "%s" and "%s"' % (len(c), r0, r1))
    for (p0, p1) in c:
        print('  %s -> %s' % (p0, p1))

def test3(repo_path, c0, c1):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)
    l = r.simple_range(c0, c1)
    for c in l:
        print('%s' % type(c))
    print('%d commits' % len(l))

def test4(repo_path, rev, path):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)
    o = r.acc(rev, path)
    print('%s' % o)

def test5(repo_path):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)

    # find origin

    b = r.blame('drivers/ieee1394/nodemgr.c', 'v2.6.20', 'v2.6.19', 766, 766)
    cid = b.for_line(766).final_commit_id
    commit = r.get_commit(cid)

    print('origin commit: %s' % commit.id)

    # check deletion

    for parent in commit.parents:
        delta = r.diff(parent, commit)
        for patch in delta:
            if patch.new_file_path == 'kernel/acct.c':
                print('parent commit: %s' % parent.id)

                old = r.get_blob(patch.old_id)
                new = r.get_blob(patch.new_id)

                old_lines = old.data.splitlines()
                new_lines = new.data.splitlines()

                for hunk in patch.hunks:
                    for (op, line) in hunk.lines:
                        if op == '-' and line.find('mutex_lock') > -1:
                            print('OK!')


def test6(repo_path):
    print('repository path: %s' % repo_path)
    r = Repository(repo_path)

    pat = re.compile(r'.*readme.*', re.I)

    def filt(fn):
        m = pat.match(fn)
        b = False
        if m:
            b = True
        return b

    l = r.find_files('HEAD', filt)

    for d in l:
        print('%s: %s' % (d['id'], d['path']))

if __name__ == '__main__':
    print
