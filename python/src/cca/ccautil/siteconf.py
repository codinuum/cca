#!/usr/bin/env python3

import os

CCA_HOME = os.getenv('CCA_HOME', '/opt/cca')

LOG_DIR = os.getenv('CCA_LOG_DIR', '/var/log/cca')

PROJECTS_DIR_NAME = 'projects'

_PROJECTS_DIR = os.path.join(CCA_HOME, PROJECTS_DIR_NAME)
PROJECTS_DIR = os.getenv('CCA_PROJECTS_DIR', _PROJECTS_DIR)

_CONFIGS_DIR = os.path.join(CCA_HOME, 'configs')
CONFIGS_DIR = os.getenv('CCA_CONFIGS_DIR', _CONFIGS_DIR)

GIT_REPO_BASE = ''

#

VIRTUOSO_DIR = '/opt/virtuoso'

SPARQL_ENDPOINT = 'http://localhost:8890/sparql'

VIRTUOSO_DSN = 'local-virtuoso'
VIRTUOSO_DRIVER = os.path.join(VIRTUOSO_DIR, 'lib/virtodbcu_r.so')
VIRTUOSO_HOST = 'localhost'
VIRTUOSO_PORT = 1111
VIRTUOSO_USER = 'dba'
VIRTUOSO_PW = 'cca'
