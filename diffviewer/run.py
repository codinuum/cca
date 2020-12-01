#!/usr/bin/env python3

import os
import sys
from subprocess import call

VIEWER_APP = 'DiffViewer-darwin-x64/DiffViewer.app'
VIEWER_CMD = VIEWER_APP + '/Contents/MacOS/DiffViewer'

if __name__ == '__main__':
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='run DiffViewer',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('file0', type=str, metavar='ORIGINAL', help='original source file')
    parser.add_argument('file1', type=str, metavar='MODIFIED', help='modified source file')

    parser.add_argument('-c', '--cache', dest='cache', default=None,
                        metavar='DIR', type=str, help='specify cache path')

    parser.add_argument('--foreground', dest='foreground', action='store_true',
                        help='do not fork')

    args = parser.parse_args()

    apath0 = os.path.abspath(args.file0)
    apath1 = os.path.abspath(args.file1)
    cache_opt = ''
    if args.cache != None:
        apathc = os.path.abspath(args.cache)
        cache_opt = ' --cache {}'.format(apathc)

    if args.foreground:
        cmd_path = os.path.join(os.path.dirname(sys.argv[0]), VIEWER_CMD)
        cmd = '{} --args{} --file0 {} --file1 {}'.format(cmd_path, cache_opt, apath0, apath1)
    else:
        app_path = os.path.join(os.path.dirname(sys.argv[0]), VIEWER_APP)
        cmd = 'open -n {} --args{} --file0 {} --file1 {}'.format(app_path, cache_opt, apath0, apath1)

    try:
        rc = call(cmd, shell=True)
    except Exception as e:
        print('failed to execute: ', e)
