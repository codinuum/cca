#!/usr/bin/env python3

'''
  A Virtuoso driver

  Copyright 2012-2023 Codinuum Software Lab <https://codinuum.com>

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
import time
import threading
import re
import logging

from . import proc

from .siteconf import (VIRTUOSO_HOST,
                       VIRTUOSO_PORT,
                       VIRTUOSO_USER,
                       VIRTUOSO_PW,
                       VIRTUOSO_DRIVER,
                       VIRTUOSO_DIR,
                       LOG_DIR)
from . import ns
from .run_workers import spawn, dump_log
# from .common import setup_logger

logger = logging.getLogger()

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


def sleep():
    # time.sleep(1)
    pass


def get_odbc_connect_string(driver=VIRTUOSO_DRIVER,
                            host=VIRTUOSO_HOST,
                            port=VIRTUOSO_PORT,
                            pwd=VIRTUOSO_PW,
                            uid=VIRTUOSO_USER):

    s = ODBC_CONNECT_STRING_FMT % {'driver': driver,
                                   'host': host,
                                   'port': port,
                                   'pwd': pwd,
                                   'uid': uid
                                   }

    return s


ODBC_CONNECT_STRING = get_odbc_connect_string(pwd=VIRTUOSO_PW)


class ODBCDriver(object):
    def __init__(self, connect_string=ODBC_CONNECT_STRING):
        self._pypyodbc_flag = False
        try:
            import pyodbc
            self._db = pyodbc.connect(connect_string,
                                      ansi=True, autocommit=True)
            self._db.setdecoding(pyodbc.SQL_CHAR, encoding='utf-8')
            self._db.setdecoding(pyodbc.SQL_WCHAR, encoding='utf-8')
            self._db.setdecoding(pyodbc.SQL_WMETADATA, encoding='utf-32le')
            self._db.setencoding(encoding='utf-8')
        except Exception as e:
            logger.warning(str(e))
            logger.warning('using pypyodbc')
            # from . import pypyodbc as pyodbc
            import pypyodbc as pyodbc
            pyodbc.lowercase = False
            self._db = pyodbc.connect((connect_string+';wideAsUTF16=1'),
                                      ansi=False, autocommit=True)
            self._pypyodbc_flag = True

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
        for row in cur.execute(query):
            vs = [d[0] for d in row.cursor_description]
            converted = ODBCDriver.conv_row(self, row)
            yield vs, converted
        cur.close()

    def execute(self, query):
        cur = self._db.cursor()
        cur.execute(query)
        cur.close()

    def fetchone(self, query):
        cur = self._db.cursor()
        row = cur.execute(query).fetchone()
        if row:
            row = ODBCDriver.conv_row(self, row)
        cur.close()
        return row


def exec_cmd(cmd):
    logger.debug(f'cmd: "{cmd}"')
    return proc.system(cmd, quiet=True)


def exec_cmd_n(cmd, n, logdir=os.curdir):
    logger.debug(f'cmd: "{cmd}"')

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


class base(object):
    def __init__(self,
                 dbdir=DB_DIR,
                 port=DEFAULT_PORT,
                 daemonize=False,
                 pw=VIRTUOSO_PW,
                 logdir=None):

        # prog_name = os.path.splitext(os.path.basename(sys.argv[0]))[0]
        # logger_name = prog_name+'.'+__name__

        if logdir is None:
            self.log_dir = os.curdir
            if daemonize:
                self.log_dir = LOG_DIR
        else:
            self.log_dir = logdir

        self._driver = None
        self._dbdir = dbdir

        self._lock_file = os.path.join(dbdir, 'virtuoso.lck')
        self._db_file = os.path.join(dbdir, 'virtuoso.db')

        self._isql_cmd_ini = f'{ISQL_CMD} {VIRTUOSO_HOST}:{port} dba dba'

        self._isql_cmd = f'{ISQL_CMD} {VIRTUOSO_HOST}:{port} {VIRTUOSO_USER} {pw}'

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
        except Exception:
            pass

        if f:
            f.close()

        return pid

    def detect_stall(self, thresh):
        mt = os.path.getmtime(self._db_file)
        nt = time.time()
        b = (nt - mt) > thresh
        logger.debug(f'{b}')
        return b

    def get_driver(self, reuse=True):
        if not self._driver or not reuse:
            connect_string = get_odbc_connect_string(pwd=self._pw,
                                                     port=self._port)
            self._driver = ODBCDriver(connect_string=connect_string)
        return self._driver

    def exec_cmd_ini(self, _cmd):
        cmd = f'{self._isql_cmd_ini} EXEC="{_cmd}"'
        rc = exec_cmd(cmd)
        sleep()
        return rc

    def exec_cmd(self, _cmd):
        cmd = f'{self._isql_cmd} EXEC="{_cmd}"'
        rc = exec_cmd(cmd)
        sleep()
        return rc

    def exec_cmd_n(self, _cmd, n):
        cmd = f'{self._isql_cmd} EXEC="{_cmd}"'
        rc = exec_cmd_n(cmd, n, logdir=self.log_dir)
        sleep()
        return rc

    def kill_server(self):
        pid = self.get_pid()
        if pid:
            cmd = f'kill {pid}'
            logger.info(f'killing virtuoso (PID={pid})...')
            exec_cmd(cmd)
            sleep()
        else:
            logger.warning('cannot obtain PID (virtuoso not running?)')

    def start_server(self):
        cmd = f'{SERVER_CMD} -c {os.path.join(self._dbdir, "virtuoso.ini")} +wait'
        return exec_cmd(cmd)

    def shutdown_server(self):
        return self.exec_cmd('shutdown')

    def set_password(self, pw):
        self._pw = pw
        return self.exec_cmd_ini(f'set password dba {pw}')

    def restart_server(self):
        self.shutdown_server()
        sleep()
        self.start_server()

    def checkpoint(self):
        return self.exec_cmd('checkpoint')

    def disable_checkpoint(self):
        return self.exec_cmd('checkpoint_interval(-1)')

    def rdfs_rule_set(self, name, graph_uri, remove=0):
        cmd = f'rdfs_rule_set(\'{name}\', \'{graph_uri}\', {remove})'
        return self.exec_cmd(cmd)

    def remove_rdfs_rule_set(self, name, graph_uri):
        self.rdfs_rule_set(name, graph_uri, remove=1)

    def clear_graph(self, graph_uri):
        cmd = f'sparql clear graph <{graph_uri}>'
        self.exec_cmd(cmd)


class Loader(base):

    def prepare_load(self, graph_uri, d, exts, resume=False):

        if resume:
            self.exec_cmd('update DB.DBA.load_list set ll_state=0 WHERE ll_state=1')

        else:
            self.exec_cmd('delete from DB.DBA.load_list')

            cmds = [f'ld_dir_all(\'{d}\', \'*{ext}\', \'{graph_uri}\')' for ext in exts]
            rc = 0
            for cmd in cmds:
                rc = self.exec_cmd(cmd)
                if rc != 0:
                    return -1
                sleep()

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

        logger.info(f'{nfiles} files are divided into {n} parts')

        rc = -1

        cmd = f'rdf_loader_run(max_files=>{maxfiles})'

        if nprocs > 1:
            proc = (lambda cmd: self.exec_cmd_n(cmd, nprocs))
        elif nprocs == 1:
            proc = (lambda cmd: self.exec_cmd(cmd))
        else:
            proc = (lambda cmd: -1)

        st = time.time()

        for i in range(n):
            logger.info(f'*** PART {i+1}/{n} ***')

            if i % RESTART_INTERVAL == 0 and i != 0:
                logger.info('restarting server...')
                self.restart_server()

            rc = proc(cmd)
            if rc != 0:
                logger.warning('Failure')
                return -1
            sleep()
            self.checkpoint()
            sleep()

        logger.info('loaded in {:.2f}s'.format(time.time() - st))

        return rc


class Dumper(base):
    def dump(self, graph_uri, dprefix):
        dump_cmd = f'dump_one_graph(\'{graph_uri}\', \'{dprefix}\')'
        rc = self.exec_cmd(dump_cmd)
        return rc
