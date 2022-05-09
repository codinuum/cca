#!/usr/bin/env python3

import sys
import os
from subprocess import check_output


def core_count():
    out = None

    if sys.platform.startswith('darwin'):
        cmd = 'sysctl -n machdep.cpu.core_count'
        try:
            _out = check_output(cmd, shell=True, encoding='utf-8')
            out = int(_out.strip())
        except Exception:
            pass

    elif sys.platform.startswith('linux'):
        cmd0 = 'grep physical.id /proc/cpuinfo | sort -u | wc -l'
        cmd1 = 'grep cpu.cores /proc/cpuinfo | sort -u'
        try:
            _out0 = check_output(cmd0, shell=True, encoding='utf-8')
            cpus = int(_out0.strip())
            _out1 = check_output(cmd1, shell=True, encoding='utf-8')
            cores = int(_out1.strip().strip('cpucores:\t '))
            out = cpus * cores
        except Exception:
            out = len(os.sched_getaffinity(0))

    return out
