#!/usr/bin/env python3

import sys
import os

#CCA_HOME = '/opt/cca'
CCA_HOME = os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0])))

VIRTUOSO_DIR = '/opt/virtuoso'

#

SPARQL_ENDPOINT = 'http://localhost:8890/sparql'

VIRTUOSO_DSN = 'local-virtuoso'
VIRTUOSO_DRIVER = os.path.join(VIRTUOSO_DIR, 'lib/virtodbcu_r.so')
VIRTUOSO_HOST = 'localhost'
VIRTUOSO_PORT = 1111
VIRTUOSO_USER = 'dba'
VIRTUOSO_PW = 'xxx'

GIT_REPO_BASE = ''

PROJECTS_DIR_NAME = 'projects'

_PROJECTS_DIR = os.path.join(CCA_HOME, PROJECTS_DIR_NAME)
PROJECTS_DIR = os.getenv('CCA_PROJECTS_DIR', _PROJECTS_DIR)
