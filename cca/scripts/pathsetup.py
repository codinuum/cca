
import os
import sys

from siteconf import CCA_HOME

HOME = os.environ['HOME']

LOG_DIR = os.path.join(CCA_HOME, 'log')

#if not os.path.exists(LOG_DIR):
#    os.makedirs(LOG_DIR)

_CONFIGS_DIR = os.path.join(CCA_HOME, 'configs')
CONFIGS_DIR = os.getenv('CCA_CONFIGS_DIR', _CONFIGS_DIR)
FACTUTILS_DIR = os.path.join(CCA_HOME, 'factutils', 'python')

dirs = [ CONFIGS_DIR, 
         FACTUTILS_DIR, 
         ]

for d in dirs:
    if d not in sys.path:
        sys.path.append(d)

