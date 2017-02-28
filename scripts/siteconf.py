
import os

HOME = os.environ['HOME']

CCA_HOME = os.path.join(HOME, 'github', 'cca')

SPARQL_ENDPOINT = 'http://localhost:8890/sparql'

VIRTUOSO_DSN = 'local-virtuoso'
VIRTUOSO_DRIVER = '/opt/virtuoso/lib/virtodbcu_r.so'
VIRTUOSO_HOST = 'localhost'
VIRTUOSO_PORT = 1111
VIRTUOSO_USER = 'dba'
VIRTUOSO_PW = 'xxx'

VIRTUOSO_DIR = '/opt/virtuoso'

GIT_REPO_BASE = ''

PROJECTS_DIR_NAME = 'projects'

_PROJECTS_DIR = os.path.join(CCA_HOME, PROJECTS_DIR_NAME)
PROJECTS_DIR = os.getenv('CCA_PROJECTS_DIR', _PROJECTS_DIR)
