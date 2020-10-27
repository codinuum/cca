#!/usr/bin/env python3

import os
import sys
from subprocess import call

if __name__ == '__main__':
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='run DiffViewer',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('file0', type=str, help='the first source file')
    parser.add_argument('file1', type=str, help='the second source file')

    parser.add_argument('-c', '--cache', dest='cache', default=None,
                        metavar='DIR', type=str, help='specify cache path')

    args = parser.parse_args()

    app_path = os.path.join(os.path.dirname(sys.argv[0]), 'DiffViewer-darwin-x64/DiffViewer.app')

    apath0 = os.path.abspath(args.file0)
    apath1 = os.path.abspath(args.file1)
    cache_opt = ''
    if args.cache != None:
        apathc = os.path.abspath(args.cache)
        cache_opt = ' --cache {}'.format(apathc)

    try:
        rc = call('open -n {} --args{} --file0 {} --file1 {}'.format(app_path, cache_opt, apath0, apath1), shell=True)
    except Exception as e:
        print('failed to execute: ', e)
