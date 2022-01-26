#!/usr/bin/env python3


'''
  fragment.py

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

import re
import hashlib
import logging

logger = logging.getLogger()


class Fragment(object):  # sorted set of GNIDs
    elem_pat = re.compile('(?P<num0>[0-9]+)(-(?P<num1>[0-9]+))?;?')

    def __init__(self, rep=''):
        self.__rep = rep  # eg. '3;5-9;12;34-45;78;90-123'
        self.__ptr = 0
        self.__cur = -1
        self.__limit = -1

    def __len__(self):
        sz = 0
        for x in self:
            sz += 1
        return sz

    def hash(self):
        m = hashlib.md5()
        m.update(self.__rep)
        h = m.hexdigest()
        return h

    def reset(self):
        self.__ptr = 0
        self.__cur = -1
        self.__limit = -1

    def __str__(self):
        return self.__rep

    def __pop_elem(self):
        result = None
        m = Fragment.elem_pat.match(self.__rep, self.__ptr)
        if m:
            n0, n1 = -1, -1
            n0s = m.group('num0')
            n1s = m.group('num1')
            if n0s:
                n0 = int(n0s)
            if n1s:
                n1 = int(n1s)

            result = (n0, n1)

            self.__ptr = m.end()
        else:
            raise StopIteration

        logger.debug(result)

        return result

    def __next__(self):
        result = None
        if self.__cur <= self.__limit and self.__cur > 0:
            result = self.__cur
            self.__cur += 1
        else:
            self.__cur = -1
            self.__limit = -1
            (num0, num1) = self.__pop_elem()
            if num1 < 0:
                result = num0
            else:
                self.__cur = num0
                self.__limit = num1
                result = self.__cur
                self.__cur += 1

        return result

    def __iter__(self):
        self.reset()
        return self

    def from_list(elems):
        result = ''
        if elems:
            prev = None
            c = 0
            for elem in elems:
                if prev is not None:
                    if elem - prev == 1:
                        c += 1
                    else:
                        if c > 0:
                            result += '-' + str(prev) + ';' + str(elem)
                            c = 0
                        else:
                            result += ';' + str(elem)
                else:
                    result += str(elem)

                prev = elem

            if c > 0:
                result += '-' + str(elems[-1])

        return Fragment(result)

    from_list = staticmethod(from_list)


def test():
    frag = Fragment('3-10;11;28')

    for x in frag:
        print(x)

    print

    print([str(x) for x in frag])


if __name__ == '__main__':
    test()
