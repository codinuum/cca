#!/usr/bin/env python3


'''
  A simple debug printing library

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


import traceback
import os
import sys

debug_flag   = False
verbose_flag = False
short_style  = True

#####

class base(object):

    def __output(self, mes, kind='', stream=sys.stdout):
        s = traceback.extract_stack()
        sf = s[-3]
        mname = sf[2]
        fname = sf[0]
        l = sf[1]

        mstr = ''
        if mname != '<module>':
            mstr = '.%s' % mname

        if short_style:
            m = os.path.basename(fname)
            stream.write('%s[%s][%s%s] %s\n' % (kind, m, self.__class__.__name__, mstr, mes))
        else:
            stream.write('%s[%s:L%s][%s%s] %s\n' % (kind, fname, l, self.__class__.__name__, mstr, mes))
    
    def debug(self, mes):
        if debug_flag:
            self.__output(mes, '[DEBUG]', stream=sys.stderr)

    def verbose(self, mes):
        if verbose_flag:
            self.__output(mes, '[VERBOSE]')

    def warning(self, mes):
        self.__output(mes, '[WARNING]', stream=sys.stderr)
        
    def error(self, mes):
        self.__output(mes, '[ERROR]', stream=sys.stderr)
        exit(1)

    def message(self, mes):
        self.__output(mes)

###

def __output(mes, kind='', stream=sys.stdout):
    s = traceback.extract_stack()
    f = s[-3]
    mname = f[2]
    fname = f[0]
    l = f[1]

    mstr = ''
    if mname != '<module>':
        mstr = '[%s]' % mname
    
    if short_style:
        m = os.path.basename(fname)
        stream.write('%s[%s]%s %s\n' % (kind, m, mstr, mes))
    else:
        stream.write('%s[%s:L%s]%s %s\n' % (kind, fname, l, mstr, mes))

def debug(mes):
    if debug_flag:
        __output(mes, '[DEBUG]')

def verbose(mes):
    if verbose_flag:
        __output(mes, '[VERBOSE]')

def warning(mes):
    __output(mes, '[WARNING]', stream=sys.stderr)

def error(mes):
    __output(mes, '[ERROR]', stream=sys.stderr)
    exit(1)
    
def message(mes):
    __output(mes)



#####


def test():
    debug_flag = True

    def f():
        debug('test')

    f()
    
    class C(base):
        def m0(self):
            self.warning("test")

        def m1(self):
            pass

    class D(C):
        def m1(self):
            self.m0()
            self.message("test")
    
    c = D()
    c.m0()
    c.m1()



if __name__ == '__main__':
    test()
