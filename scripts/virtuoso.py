#!/usr/bin/env python3

'''
  A Virtuoso driver

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
import subprocess
import time
import threading
import re
import sys

import pathsetup
from pathsetup import LOG_DIR
import dp
import log
import proc
from siteconf import (VIRTUOSO_HOST,
                      VIRTUOSO_PORT,
                      VIRTUOSO_USER,
                      VIRTUOSO_PW,
                      VIRTUOSO_DRIVER,
                      VIRTUOSO_DSN,
                      VIRTUOSO_DIR)
import ns
from run_workers import spawn, dump_log


DEFAULT_MAX_FILES = 500

RESTART_INTERVAL = 100

DEFAULT_PORT = 1111

###

GRAPH_URI_BASE = ns.FB_NS

VIRTUOSO_BIN_DIR = os.path.join(VIRTUOSO_DIR, 'bin')
DB_DIR = os.path.join(VIRTUOSO_DIR, 'database')
VTMP_DIR = os.path.join(VIRTUOSO_DIR, 'tmp')

SERVER_CMD = os.path.join(VIRTUOSO_BIN_DIR, 'virtuoso-t')
ISQL_CMD = os.path.join(VIRTUOSO_BIN_DIR, 'isql-v')

ODBC_CONNECT_STRING_FMT = 'Driver=%(driver)s;HOST=%(host)s:%(port)d;UID=%(uid)s;PWD=%(pwd)s'

def get_odbc_connect_string(driver=VIRTUOSO_DRIVER,
                            host=VIRTUOSO_HOST,
                            port=VIRTUOSO_PORT,
                            pwd=VIRTUOSO_PW,
                            uid=VIRTUOSO_USER):

    s = ODBC_CONNECT_STRING_FMT % {'driver':driver,
                                   'host':host,
                                   'port':port,
                                   'pwd':pwd,
                                   'uid':uid}

    return s

ODBC_CONNECT_STRING = get_odbc_connect_string(pwd=VIRTUOSO_PW)


class ODBCDriver(dp.base):
    def __init__(self, connect_string=ODBC_CONNECT_STRING):
        self.message('using pypyodbc')
        import pypyodbc as pyodbc
        pyodbc.lowercase = False
        self._db = pyodbc.connect(connect_string.encode('utf-8'), ansi=True, autocommit=True)

    def conv_row(self, row):
        d = {}
        idx = 0
        for desc in row.cursor_description:
            lab = desc[0]
            v = row[idx]
            d[lab] = v
            idx += 1
        return d

    def query(self, query):
        cur = self._db.cursor()
        for row in cur.execute(query.encode('utf-8')):
            vs = [d[0] for d in row.cursor_description]
            converted = ODBCDriver.conv_row(self, row)
            yield vs, converted
        cur.close()

    def execute(self, query):
        cur = self._db.cursor()
        cur.execute(query.encode('utf-8'))
        cur.close()

    def fetchone(self, query):
        cur = self._db.cursor()
        row = cur.execute(query.encode('utf-8')).fetchone()
        if row:
            row = ODBCDriver.conv_row(self, row)
        cur.close()
        return row


def exec_cmd(cmd):
    dp.debug('cmd: "%s"' % cmd)

    return proc.system(cmd, quiet=True)


def exec_cmd_n(cmd, n, logdir='.'):
    dp.debug('cmd: "%s"' % cmd)

    ps = []
    out_tbl = {}

    for i in range(n):
        wid = str(i)
        p = spawn(cmd)
        out_tbl[wid] = (p.stdout, p.stderr) 
        ps.append(p)

    for wid in out_tbl.keys():
        th = threading.Thread(target=dump_log, args=('virtuoso', 
                                                     wid, 
                                                     out_tbl[wid], 
                                                     logdir))
        th.start()

    rc = 0

    for p in ps:
        rc0 = p.wait()
        if rc0 != 0:
            rc = rc0

    return rc


PID_PAT = re.compile('VIRT_PID=(?P<pid>[0-9]+)')

class base(log.Logger):
    def __init__(self,
                 dbdir=DB_DIR,
                 port=DEFAULT_PORT,
                 daemonize=False,
                 pw=VIRTUOSO_PW):

        prog_name = os.path.splitext(os.path.basename(sys.argv[0]))[0]
        logger_name = prog_name+'.'+__name__

        log_dir = None
        if daemonize:
            log_dir = LOG_DIR

        log.Logger.__init__(self, logger_name, log_dir)

        self._driver = None
        self._dbdir = dbdir

        self._lock_file = os.path.join(dbdir, 'virtuoso.lck')
        self._db_file = os.path.join(dbdir, 'virtuoso.db')

        self._isql_cmd_ini = '%s %s:%d dba dba' % (ISQL_CMD, VIRTUOSO_HOST, port)

        self._isql_cmd = '%s %s:%d %s' % (ISQL_CMD,
                                          VIRTUOSO_HOST,
                                          port,
                                          VIRTUOSO_USER)

        self._pw = pw
        self._port = port


    def get_pid(self):
        pid = None
        f = None
        try:
            f = open(self._lock_file, 'r')
            for line in f.readlines():
                m = PID_PAT.match(line.strip())
                if m:
                    pid = m.group('pid')
                    break
        except:
            pass

        if f:
            f.close()

        return pid


    def detect_stall(self, thresh):
        mt = os.path.getmtime(self._db_file)
        nt = time.time()
        b = (nt - mt) > thresh
        dp.debug('%s' % b)
        return b


    def get_driver(self, reuse=True):
        if not self._driver or not reuse:
            connect_string = get_odbc_connect_string(pwd=self._pw, port=self._port)
            self._driver = ODBCDriver(connect_string=connect_string)
        return self._driver

    def exec_cmd_ini(self, _cmd):
        cmd = '%s EXEC="%s"' % (self._isql_cmd_ini, _cmd)
        rc = exec_cmd(cmd)
        time.sleep(1)
        return rc

    def exec_cmd(self, _cmd):
        cmd = '%s %s EXEC="%s"' % (self._isql_cmd, self._pw, _cmd)
        rc = exec_cmd(cmd)
        time.sleep(1)
        return rc

    def exec_cmd_n(self, _cmd, n):
        cmd = '%s %s EXEC="%s"' % (self._isql_cmd, self._pw, _cmd)
        rc = exec_cmd_n(cmd, n, logdir=LOG_DIR)
        time.sleep(1)
        return rc

    def kill_server(self):
        pid = self.get_pid()
        if pid:
            cmd = 'kill %s' % pid
            self.message('killing virtuoso (PID=%s)...' % pid)
            exec_cmd(cmd)
            time.sleep(3)
        else:
            self.warning('cannot obtain PID (virtuoso not running?)')

    def start_server(self):
        cmd = '%s -c %s +wait' % (SERVER_CMD,
                                  os.path.join(self._dbdir, 'virtuoso.ini'))
        return exec_cmd(cmd)

    def shutdown_server(self):
        return self.exec_cmd('shutdown')

    def set_password(self, pw):
        self._pw = pw
        return self.exec_cmd_ini('set password dba %s' % pw)

    def restart_server(self):
        self.shutdown_server()
        time.sleep(1)
        self.start_server()

    def checkpoint(self):
        return self.exec_cmd('checkpoint')

    def disable_checkpoint(self):
        return self.exec_cmd('checkpoint_interval(-1)')

    def rdfs_rule_set(self, name, graph_uri, remove=0):
        cmd = 'rdfs_rule_set(\'%s\', \'%s\', %d)' % (name, graph_uri, remove)
        return self.exec_cmd(cmd)

    def remove_rdfs_rule_set(self, name, graph_uri):
        self.rdfs_rule_set(name, graph_uri, remove=1)

    def clear_graph(self, graph_uri):
        cmd = 'sparql clear graph <%s>' % graph_uri
        self.exec_cmd(cmd)




class Loader(base):

    def prepare_load(self, graph_uri, d, exts, resume=False):

        if resume:
            self.exec_cmd('update DB.DBA.load_list set ll_state=0 WHERE ll_state=1')

        else:
            self.exec_cmd('delete from DB.DBA.load_list')

            cmds = ['ld_dir_all(\'%s\', \'*%s\', \'%s\')' % (d, ext, graph_uri) for ext in exts]
            rc = 0
            for cmd in cmds:
                rc = self.exec_cmd(cmd)
                if rc != 0:
                    return -1
                time.sleep(1)

            self.checkpoint()

        driver = self.get_driver()
        row = driver.fetchone('SELECT COUNT(*) FROM DB.DBA.load_list WHERE ll_state=0')
        nfiles = row['count']
        return nfiles


    def load(self, graph_uri, d, exts, nprocs=1, maxfiles=DEFAULT_MAX_FILES,
             resume=False):

        nfiles = self.prepare_load(graph_uri, d, exts, resume=resume)

        n = int(nfiles / maxfiles / nprocs)
        if nfiles % (maxfiles * nprocs) > 0:
            n += 1

        self.message('{} files are divided into {} parts'.format(nfiles, n))

        rc = -1

        cmd = 'rdf_loader_run(max_files=>%d)' % maxfiles

        if nprocs > 1:
            proc = lambda cmd: self.exec_cmd_n(cmd, nprocs)
        elif nprocs == 1:
            proc = lambda cmd: self.exec_cmd(cmd)
        else:
            proc = lambda cmd: -1

        for i in range(n):
            self.message('*** PART %d/%d ***' % (i+1, n))

            if i % RESTART_INTERVAL == 0 and i != 0:
                self.message('restarting server...')
                self.restart_server()

            rc = proc(cmd)
            if rc != 0:
                self.warning('Failure')
                return -1
            time.sleep(1)
            self.checkpoint()
            time.sleep(1)

        return rc



class Dumper(base):
    def dump(self, graph_uri, dprefix):
        dump_cmd = 'dump_one_graph(\'%s\', \'%s\')' % (graph_uri, dprefix)
        rc = self.exec_cmd(dump_cmd)
        return rc
