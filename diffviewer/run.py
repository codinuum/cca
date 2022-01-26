#!/usr/bin/env python3

import os
import sys
import re
from subprocess import call

VIEWER_APP_DIR_PAT = re.compile(r'DiffViewer-darwin-([0-9a-z]+)')
VIEWER_CMD_PATH = '/Contents/MacOS/DiffViewer'

if __name__ == '__main__':
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='run DiffViewer',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('file0', type=str, metavar='ORIGINAL',
                        help='original source file')

    parser.add_argument('file1', type=str, metavar='MODIFIED',
                        help='modified source file')

    parser.add_argument('-c', '--cache', dest='cache', default=None,
                        metavar='DIR', type=str, help='specify cache path')

    parser.add_argument('--foreground', dest='foreground', action='store_true',
                        help='do not fork')

    args = parser.parse_args()

    apath0 = os.path.abspath(args.file0)
    apath1 = os.path.abspath(args.file1)
    opt = ''
    if args.cache is not None:
        apathc = os.path.abspath(args.cache)
        opt = ' --cache {}'.format(apathc)

    here = os.path.dirname(sys.argv[0])
    viewer_app_path = None

    for fn in os.listdir(here):
        if VIEWER_APP_DIR_PAT.match(fn):
            viewer_app_path = os.path.join(here, fn, 'DiffViewer.app')

    if viewer_app_path is None:
        print('App not found')
    else:
        if args.foreground:
            cmdp = os.path.join(viewer_app_path, VIEWER_CMD_PATH)
            cmd = f'{cmdp} --args{opt} --file0 {apath0} --file1 {apath1}'
        else:
            app = os.path.join(viewer_app_path)
            cmd = f'open -n {app} --args{opt} --file0 {apath0} --file1 {apath1}'

        try:
            rc = call(cmd, shell=True)
        except Exception as e:
            print('failed to execute: ', e)
