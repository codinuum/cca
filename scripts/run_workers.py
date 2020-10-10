#!/usr/bin/env python3


'''
  A script for running worker processes

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
import sys
import subprocess
import threading
import select
import time

import pathsetup
import dp

#####

TIMEOUT = 5
BUFSIZE = 0 # unbuffered
LOG_BUFSIZE = 256

#####

def spawn(cmd):
    sproc = subprocess.Popen(cmd, 
                             bufsize=BUFSIZE, 
                             shell=True, 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE, 
                             close_fds=True)
    return sproc

def mklogname(cmd_name, wid):
    (base, ext) = os.path.splitext(cmd_name)
    return 'log.worker.%s.%s' % (base, wid)


def dump_log(cmd_name, wid, sout_serr, wdir='.', timeout=TIMEOUT):
    sout, serr = sout_serr
    log = os.path.join(wdir, mklogname(cmd_name, wid))

    f = open(log, 'w')

    running = True

    outs = [sout, serr]

    nouts = len(outs)

    max_count = LOG_BUFSIZE
    prev_out = None
    out = None
    count = 0
    
    nclosed = 0

    while running:
        try:
            (ready_outs, x0, x1) = select.select(outs, [], [], timeout)

            n = len(ready_outs)

            if prev_out:
                if prev_out in ready_outs:
                    if count < max_count:
                        out = prev_out
                        count += 1
                    else:
                        if n > 1:
                            i = ready_outs.index(prev_out)
                            if i < n - 1:
                                out = ready_outs[i+1]
                            else:
                                out = ready_outs[0]
                            count = 0
                        else:
                            out = prev_out
                elif n > 0:
                    out = ready_outs[0]
                    count = 0
                else:
                    out = None
            elif n > 0:
                out = ready_outs[0]
                count = 0
            else:
                out = None
                
            prev_out = out

            if out:
                dat = out.read(1)

                if dat:
                    f.write(dat)

                else:
                    outs.remove(out)
                    nclosed += 1

            if nclosed >= nouts:
                running = False
                dp.message('[wid:%s] finished.' % wid)

        except BaseException as e:
            print(str(e))
            break

    f.close()

def store_carg(option, opt_str, value, parser):
    setattr(parser.values, option.dest, getattr(parser.values, option.dest)+' '+value)


def main():
    from optparse import OptionParser

    usage = 'usage: %prog [OPTIONS] [TARGET_DIR]'

    optparser = OptionParser(usage)

    optparser.add_option('-c', '--cmd', dest='cmd', 
                         help='set command to CMD', metavar='CMD')

    optparser.add_option('-a', '--arg', action='callback', callback=store_carg, nargs=1, 
                         dest='cargs', type='string', default='',
                         help='set ARGS for sub command', metavar='ARGS')

    optparser.add_option('-n', '--nprocs', dest='nprocs', action='store', type='int',
                         help='set nprocs to N', metavar='N', default=2)

    optparser.add_option('-d', '--debug', action='store_true', dest='debug',
                         help='enable debug output')

    (opt, args) = optparser.parse_args()

    if opt.debug:
        dp.debug_flag = True

    target_dir = '.'

    if args:
        target_dir = args[0]

    dist_dir = os.path.dirname(sys.argv[0])


    dp.message('command: "%s"' % opt.cmd)

    w_cmd = ''

    if opt.cmd:
        w_cmd = os.path.join(dist_dir, opt.cmd)
    else:
        dp.error('no command specified')


    out_tbl = {}

    for i in range(opt.nprocs):
        wid = str(i)
        dp.message('worker id: %s' % wid)

        if w_cmd:
            arg = ''
            if opt.cargs:
                arg = opt.cargs

            engine_opt = ''
#            if opt.engine:
#                engine_opt = '-e %s' % opt.engine

            cmd = '%s %s -c work -w %s %s %s' % (w_cmd, arg, wid, engine_opt, target_dir)
            
            dp.message('cmd: "%s"' % cmd)

            p = spawn(cmd)
            time.sleep(1)

            out_tbl[wid] = (p.stdout, p.stderr) 


    for wid in out_tbl.keys():
        th = threading.Thread(target=dump_log, args=(opt.cmd, wid, out_tbl[wid], target_dir))
        th.start()


if __name__ == '__main__':
    main()
