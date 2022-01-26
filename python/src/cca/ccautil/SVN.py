#!/usr/bin/env/python3

'''
  SVN.py

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
import shutil
import pysvn
from urllib.request import url2pathname
from urllib.parse import urlparse
import logging

logger = logging.getLogger()


class Item(object):
    def __init__(self, item):
        self._item = item

    def __str__(self):
        return '{} {} {}'.format(self.get_path(), self._item.summarize_kind,
                                 self._item.node_kind)

    def is_added(self):
        return self._item.summarize_kind == pysvn.diff_summarize_kind.added

    def is_deleted(self):
        return self._item.summarize_kind == pysvn.diff_summarize_kind.delete

    def is_modified(self):
        return self._item.summarize_kind == pysvn.diff_summarize_kind.modified

    def is_file(self):
        return self._item.node_kind == pysvn.node_kind.file

    def is_dir(self):
        return self._item.node_kind == pysvn.node_kind.dir

    def get_path(self):
        return self._item.path


def wrap_item(i):
    w = None
    if i:
        w = Item(i)
    return w


class Repository(object):

    def __init__(self, url, username='', password=''):
        os.umask(0o0002)
        self._svn_url = url
        # logger.info('URL="{}"'.format(self._svn_url))

        self._svn_username = username
        self._svn_password = password

        self._ssl_server_trust_prompt = None

        self._setup_cli()

    def set_ssl_server_trust_prompt(self, f):
        self._ssl_server_trust_prompt = f

    def _cb_notify(self, ev):
        if 'error' in ev:
            mesg = ev['error']
            if mesg:
                print(mesg)

    def init(self):
        self._setup_cli()

    def _setup_cli(self):
        self.svn_cli = pysvn.Client()
        self.svn_cli.exception_style = 1
        if self._svn_username:
            def get_login(r, u, s):
                return (True, self._svn_username, self._svn_password, False)
            self.svn_cli.callback_get_login = get_login

        self.svn_cli.callback_notify = self._cb_notify

        if self._ssl_server_trust_prompt:
            self.svn_cli.callback_ssl_server_trust_prompt = \
                self._ssl_server_trust_prompt

    def get_head_rev(self):
        entries = self.svn_cli.info2(self._svn_url,
                                     revision=pysvn.Revision(pysvn
                                                             .opt_revision_kind
                                                             .head),
                                     recurse=False)
        for (p, info) in entries:
            rev = info.rev
            return rev
        return None

    def get_root_url(self, revnum=None):
        rev = None
        if revnum:
            rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
        else:
            rev = pysvn.Revision(pysvn.opt_revision_kind.head)

        entries = self.svn_cli.info2(self._svn_url, revision=rev,
                                     recurse=False)

        for (p, info) in entries:
            root = info.repos_root_URL
            return root

        return None

    def get_kind(self, item, revnum):
        rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
        try:
            entries = self.svn_cli.info2(self._svn_url+'/'+item, revision=rev,
                                         recurse=False)
            for (p, info) in entries:
                kind = info.kind
                return kind
            return pysvn.node_kind.unknown

        except pysvn.ClientError as e:
            for (mes, code) in e.args[1]:
                if code == pysvn.svn_err.ra_illegal_url:
                    return pysvn.node_kind.none

            logger.error('failed: {}'.format(e))

    def is_file(self, item, revnum):
        k = self.get_kind(item, revnum)
        return k == pysvn.node_kind.file

    def get_diff(self, revnum1, revnum2, outfile=None, options=['-u']):
        rev1 = pysvn.Revision(pysvn.opt_revision_kind.number, revnum1)
        rev2 = pysvn.Revision(pysvn.opt_revision_kind.number, revnum2)

        # You should setup subversion to use an external diff command since the internal one is buggy!
        delta = self.svn_cli.diff('/tmp',
                                  self._svn_url, rev1,
                                  self._svn_url, rev2,
                                  diff_options=options,
                                  # use_git_diff_format=git,
                                  )

        if delta != '' and outfile is not None:
            f = None
            try:
                f = open(outfile, 'w')
                f.write(delta)
            except IOError as e:
                logger.warning(str(e))
            finally:
                if f:
                    f.close()

        return delta

    def get_changed_items(self, revnum1, revnum2):
        rev1 = pysvn.Revision(pysvn.opt_revision_kind.number, revnum1)
        rev2 = pysvn.Revision(pysvn.opt_revision_kind.number, revnum2)
        _items = self.svn_cli.diff_summarize(self._svn_url, rev1,
                                             self._svn_url, rev2)
        items = [wrap_item(i) for i in _items]
        return items

    def get_all_items(self, revnum=None):
        rev = None

        if revnum:
            rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
        else:
            rev = pysvn.Revision(pysvn.opt_revision_kind.head)

        entries = self.svn_cli.list(self._svn_url, rev, recurse=True)

        root_url = self.get_root_url(revnum)
        prefix = '/' + (self._svn_url.replace(root_url, '', 1).lstrip('/'))

        items = [e[0].repos_path.replace(prefix, '', 1).lstrip('/')
                 for e in entries]

#         print 'root_url: {}'.format(root_url)
#         print 'prefix: {}'.format(prefix)
#         print 'items:'
#         for i in items:
#             print i

        return items

    def checkout_file(self, item, dest, revnum=None, verbose=False):
        revnum_s = str(revnum)
        if revnum is None:
            revnum_s = 'head'

        url = self._svn_url

        d = '/'.join(item.split('/')[:-1])

        path = os.path.join(dest, url2pathname(item))
        fullurl = url+'/'+item

        if d:
            url = url + '/' + d
            dest = os.path.join(dest, url2pathname(d))

        if urlparse(url).fragment:
            logger.warning('invalid url: {}'.format(url))

        else:
            rev = None

            if revnum:
                rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
            else:
                rev = pysvn.Revision(pysvn.opt_revision_kind.head)

            if verbose:
                logger.info('checking out "{}@{}" to "{}"'.format(fullurl,
                                                                  revnum_s,
                                                                  path))

            if os.path.exists(path):
                if os.path.isdir(path):
                    pass
                else:
                    logger.warning('already exist: "{}", removing..'.format(path))
                    os.unlink(path)

            if not os.path.exists(os.path.join(dest, '.svn')):
                self.svn_cli.checkout(url, dest, revision=rev,
                                      ignore_externals=True,
                                      depth=pysvn.depth.empty)

            self.svn_cli.update(path, revision=rev, ignore_externals=True,
                                depth=pysvn.depth.files)

    def checkout(self, dest, revnum=None, directory=None, verbose=False):
        revnum_s = str(revnum)
        if revnum is None:
            revnum_s = 'head'

        url = self._svn_url
        if directory:
            url += '/' + directory
            dest = os.path.join(dest, url2pathname(directory))

        if verbose:
            logger.info('checking out "{}@{}" to "{}"'.format(url, revnum_s,
                                                              dest))

        if os.path.exists(dest):
            logger.warning('already exist: "{}", removing..'.format(dest))
            shutil.rmtree(dest)

        rev = None

        if revnum:
            rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
        else:
            rev = pysvn.Revision(pysvn.opt_revision_kind.head)

        self.svn_cli.checkout(url,
                              dest,
                              recurse=True,
                              revision=rev,
                              ignore_externals=False)

    def mkdir(self, d, verbose=False):
        if verbose:
            logger.info('creating "{}"'.format(d))

        if not os.path.exists(d):
            try:
                os.mkdir(d)
            except OSError:
                p = os.path.dirname(d)
                self.mkdir(p)
                os.mkdir(d)

    def checkout_source(self, path, revnum=None, verbose=False):
        revnum_s = str(revnum)
        if revnum is None:
            revnum_s = 'head'

        url = self._svn_url

        if verbose:
            logger.info('checking out "{}@{}" as {}'.format(url, revnum_s, path))

        if os.path.exists(path):
            logger.warning('already exist: "{}"'.format(path))
        else:
            if revnum:
                rev = pysvn.Revision(pysvn.opt_revision_kind.number, revnum)
            else:
                rev = pysvn.Revision(pysvn.opt_revision_kind.head)

            text = self.svn_cli.cat(url, revision=rev)
            f = None
            d = os.path.dirname(path)
            try:
                if not os.path.exists(d):
                    self.mkdir(d)
                f = open(path, 'w')
                f.write(text)
            finally:
                if f:
                    f.close()


def get_log(path):
    lmap = {}

    cli = pysvn.Client()

    try:
        logs = cli.log(path)

        for log in logs:
            author = log.get('author', '<unknown>')
            _message = log.get('message', None)
            _rev = log.get('revision', None)
            if _rev is not None and _message is not None:
                rev = _rev.number
                message = _message.lstrip().rstrip()
                try:
                    rmap = lmap[author]
                    try:
                        messages = rmap[rev]
                        messages.add(message)
                    except KeyError:
                        rmap[rev] = {message}
                except KeyError:
                    lmap[author] = {rev: {message}}

    except pysvn.ClientError as e:
        logger.warning(str(e))

    return lmap


def blame(path):
    amap = {}  # author -> rev -> range list

    cli = pysvn.Client()

    try:
        annot = cli.annotate(path)

        for d in annot:
            author = d['author']
            if author == '':
                author = '<unknown>'
            _rev = d.get('revision', None)

            if _rev is None:
                continue

            rev = _rev.number

#            print('rev={} author={}'.format(rev, author))

            num = d['number'] + 1
            last_col = max(len(d['line']) - 1, 0)

            rmap = amap.get(author, {})
            ranges = rmap.get(rev, [])

            new_ranges = []
            modified = False

            for (st, ed, lcol) in ranges:
                if num == ed + 1:
                    new_ranges.append((st, num, last_col))
                    modified = True

                elif num == st - 1:
                    new_ranges.append((num, ed, lcol))
                    modified = True

                else:
                    new_ranges.append((st, ed, lcol))

            if not modified:
                new_ranges.append((num, num, last_col))

            rmap[rev] = new_ranges
            amap[author] = rmap

    except pysvn.ClientError as e:
        logger.warning(str(e))

    return amap
