#!/usr/bin/env python3


'''
  sim.py

  Copyright 2012-2021 Codinuum Software Lab <https://codinuum.com>

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

import filecmp
import difflib

from . import java_token_diff as java


def line_sim(f1, f2):
    if filecmp.cmp(f1, f2):
        return 0.0
    lines1 = open(f1, encoding='utf-8', errors='replace').readlines()
    lines2 = open(f2, encoding='utf-8', errors='replace').readlines()
    matcher = difflib.SequenceMatcher(None, lines1, lines2)
    similarity = matcher.quick_ratio()
    return similarity


def java_sim(f1, f2):
    if filecmp.cmp(f1, f2):
        return 0.0
    toks1 = java.get_tokens(f1)
    toks2 = java.get_tokens(f2)
    matcher = difflib.SequenceMatcher(isjunk=None, a=toks1, b=toks2)
    similarity = matcher.quick_ratio()
    return similarity


def sim(f1, f2, plain=False):
    similarity = 1.0
    if not filecmp.cmp(f1, f2):
        if plain:
            similarity = line_sim(f1, f2)
        elif java.is_src(f1) and java.is_src(f2):
            similarity = java_sim(f1, f2)
        else:
            similarity = line_sim(f1, f2)

    return similarity


def main():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

    parser = ArgumentParser(description='compute similarity between files',
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('path1', type=str)
    parser.add_argument('path2', type=str)

    parser.add_argument('--plain', dest='plain', action='store_true',
                        help='perform language agnostic differencing')

    args = parser.parse_args()

    try:
        s = sim(args.path1, args.path2, plain=args.plain)
        print(s)
    except IOError as e:
        print('ERROR: {}'.format(str(e)))


if __name__ == '__main__':
    main()
