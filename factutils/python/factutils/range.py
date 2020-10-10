#!/usr/bin/env python3

'''
  Factutils: helper scripts for source code entities

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

from functools import reduce

from .const import SUB_SEP

import pathsetup
import dp


def compo_to_int(s):
    i = -1
    try:
        i = int(s)
    except Exception as e:
        if s != 'U':
            dp.warning(str(e))
    return i


class Range(dp.base):
    def __init__(self):
        self._valid = False
        self._encoded = None
        self._enc = None

    def is_valid(self):
        return self._valid

    def encode(self):
        return self._encoded

    def get_enc(self):
        return self._enc

    def meet(self, other):
        self.warning('not implemented')
        return None

    def overlaps(self, other):
        return self.meet(other) != None

    def contains(self, other):
        self.warning('not implemented')
        return None



class LCRange(Range):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        obj = None
        try:
            sl = compo_to_int(compos[0])
            sc = compo_to_int(compos[1])
            el = compo_to_int(compos[2])
            ec = compo_to_int(compos[3])
            obj = LCRange(sl, sc, el, ec)
        except Exception as e:
            dp.warning(str(e))
        return obj

    def __init__(self, sl, sc, el, ec):
        Range.__init__(self)
        if sl == el:
            self._valid = sc <= ec
        else:
            self._valid = sl < el
        self._start_line = sl
        self._start_col = sc
        self._end_line = el
        self._end_col = ec
        self._enc = 'LC'
        self._encoded = SUB_SEP.join([str(x) for x in [self._start_line, 
                                                       self._start_col, 
                                                       self._end_line, 
                                                       self._end_col]])

    def __eq__(self, other):
        res = False
        if isinstance(other, LCRange):
            res = reduce(lambda x,y: x and y, [self._start_line == other._start_line,
                                               self._start_col == other._start_col,
                                               self._end_line == other._end_line,
                                               self._end_col == other._end_col])
        return res


    @classmethod
    def _meet(cls, s0, s1):
        sl0, sc0, el0, ec0 = s0
        sl1, sc1, el1, ec1 = s1
        sl = max(sl0, sl1)
        el = min(el0, el1)

        sc = 0
        if el0 == el1:
            sc = max(sc0, sc1)
        else:
            if sl0 > sl1:
                sc = sc0
            else:
                sc = sc1

        ec = 0
        if el0 == el1:
            ec = min(ec0, ec1)
        else:
            if el0 > el1:
                ec = ec1
            else:
                ec = ec0

        return (sl, sc, el, ec)

    def __str__(self):
        s = '%dL,%dC-%dL,%dC' % (self._start_line, 
                                 self._start_col, 
                                 self._end_line, 
                                 self._end_col)
        return s
        
    def meet(self, other):
        m = None
        if isinstance(other, LCRange):
            (sl, sc, el, ec) = LCRange._meet((self._start_line, 
                                              self._start_col, 
                                              self._end_line, 
                                              self._end_col), 
                                             (other._start_line,
                                              other._start_col,
                                              other._end_line,
                                              other._end_col))

            m = LCRange(sl, sc, el, ec)
            if not m.is_valid():
                m = None
            
        return m

    def contains(self, other):
        b = False
        if isinstance(other, LCRange):
            b = self._start_line <= other._start_line and other._end_line <= self._end_line and self._start_col <= other._start_col and other._end_col <= self._end_col
        return b

    def get_start_line(self):
        return self._start_line
    def get_start_col(self):
        return self._start_col
    def get_end_line(self):
        return self._end_line
    def get_end_col(self):
        return self._end_col


class ORange(Range):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        obj = None
        try:
            so = compo_to_int(compos[0])
            eo = compo_to_int(compos[1])
            obj = ORange(so, eo)
        except Exception as e:
            dp.warning(str(e))
        return obj

    def __init__(self, so, eo):
        Range.__init__(self)
        self._valid = so <= eo
        self._start_offset = so
        self._end_offset = eo
        self._enc = 'O'
        self._encoded = SUB_SEP.join([str(x) for x in [self._start_offset,
                                                       self._end_offset]])

    def __eq__(self, other):
        res = False
        if isinstance(other, ORange):
            res = reduce(lambda x,y: x and y, [self._start_offset == other._start_offset,
                                               self._end_offset == other._end_offset])
        return res

    def __str__(self):
        s = '%d-%d' % (self._start_offset, 
                       self._end_offset)
        return s

    def meet(self, other):
        m = None
        if isinstance(other, ORange):
            so = max(self._start_offset, other._start_offset)
            eo = min(self._end_offset, other._end_offset)
            m = ORange(so, eo)
            if not m.is_valid():
                m = None

        return m

    def contains(self, other):
        b = False
        if isinstance(other, ORange) or isinstance(other, LCORange):
            b = self._start_offset <= other._start_offset and other._end_offset <= self._end_offset
        return b


    def get_start_offset(self):
        return self._start_offset
    def get_end_offset(self):
        return self._end_offset


class LORange(ORange):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        obj = None
        try:
            sl = compo_to_int(compos[0])
            so = compo_to_int(compos[2])
            el = compo_to_int(compos[3])
            eo = compo_to_int(compos[5])
            obj = LORange(sl, so, el, eo)
        except Exception as e:
            dp.warning(str(e))
        return obj
        
    def __init__(self, sl, so, el, eo):
        Range.__init__(self)
        valid0 = sl <= el and so <= eo
        self._start_line = sl
        self._end_line = el
        ORange.__init__(self, so, eo)
        self._valid = self._valid and valid0
        self._enc = 'LO'
        self._encoded = SUB_SEP.join([str(x) for x in [self._start_line, 
                                                       self._start_offset,
                                                       self._end_line, 
                                                       self._end_offset]])


    def __eq__(self, other):
        res = False
        if isinstance(other, LORange):
            res = reduce(lambda x,y: x and y, [self._start_line == other._start_line,
                                               self._start_offset == other._start_offset,
                                               self._end_line == other._end_line,
                                               self._end_offset == other._end_offset])

    def __str__(self):
        s = '%dL(%d)-%dL(%d)' % (self._start_line, 
                                 self._start_offset,
                                 self._end_line, 
                                 self._end_offset)
        return s


    def meet(self, other):
        m = None
        if isinstance(other, LORange):
            sl = max(self._start_line, other._start_line)
            el = min(self._end_line, other._end_line)

            so = max(self._start_offset, other._start_offset)
            eo = min(self._end_offset, other._end_offset)
            m = LORange(sl, so, el, eo)
            if not m.is_valid():
                m = None

        return m

    def contains(self, other):
        b = False
        if isinstance(other, ORange) or isinstance(other, LORange):
            b = self._start_offset <= other._start_offset and other._end_offset <= self._end_offset
        return b


class LCORange(LCRange, ORange):
    @classmethod
    def from_encoded(cls, encoded):
        compos = encoded.split(SUB_SEP)
        obj = None
        try:
            sl = compo_to_int(compos[0])
            sc = compo_to_int(compos[1])
            so = compo_to_int(compos[2])
            el = compo_to_int(compos[3])
            ec = compo_to_int(compos[4])
            eo = compo_to_int(compos[5])
            obj = LCORange(sl, sc, so, el, ec, eo)
        except Exception as e:
            dp.warning(str(e))
        return obj
        
    def __init__(self, sl, sc, so, el, ec, eo):
        LCRange.__init__(self, sl, sc, el, ec)
        valid0 = self._valid
        ORange.__init__(self, so, eo)
        self._valid = self._valid and valid0
        self._enc = 'LCO'
        self._encoded = SUB_SEP.join([str(x) for x in [self._start_line, 
                                                       self._start_col, 
                                                       self._start_offset,
                                                       self._end_line, 
                                                       self._end_col,
                                                       self._end_offset]])


    def __eq__(self, other):
        res = False
        if isinstance(other, LCORange):
            res = reduce(lambda x,y: x and y, [self._start_line == other._start_line,
                                               self._start_col == other._start_col,
                                               self._start_offset == other._start_offset,
                                               self._end_line == other._end_line,
                                               self._end_col == other._end_col,
                                               self._end_offset == other._end_offset])

    def __str__(self):
        s = '%dL,%dC(%d)-%dL,%dC(%d)' % (self._start_line, 
                                         self._start_col, 
                                         self._start_offset,
                                         self._end_line, 
                                         self._end_col,
                                         self._end_offset)
        return s


    def meet(self, other):
        m = None
        if isinstance(other, LCORange):
            slc = (self._start_line, 
                   self._start_col, 
                   self._end_line, 
                   self._end_col)

            olc = (other._start_line,
                   other._start_col,
                   other._end_line,
                   other._end_col)

            (sl, sc, el, ec) = LCRange._meet(slc, olc)

            so = max(self._start_offset, other._start_offset)
            eo = min(self._end_offset, other._end_offset)
            m = LCORange(sl, sc, so, el, ec, eo)
            if not m.is_valid():
                m = None

        return m

    def contains(self, other):
        b = False
        if isinstance(other, ORange) or isinstance(other, LCORange):
            b = self._start_offset <= other._start_offset and other._end_offset <= self._end_offset
        return b


class MaxRange(Range):
    def __init__(self):
        Range.__init__(self)
        self._valid = True
        self._enc = 'MAX'
        self._encoded = ''

    def __eq__(self, other):
        return isinstance(other, MaxRange)

    def __str__(self):
        return '<max range>'

    def contains(self, other):
        return True

    def get_start_line(self):
        return 1
    def get_start_col(self):
        return 0
    def get_end_line(self):
        return -1
    def get_end_col(self):
        return 0
    def get_start_offset(self):
        return 0
    def get_end_offset(self):
        return -1

MAX_RANGE = MaxRange()

def from_encoded(encoded):
    range = None
    compos = encoded.split(SUB_SEP)
    n = len(compos)
    if n == 6:
        range = LCORange.from_encoded(encoded)

    elif n == 4:
        range = LCRange.from_encoded(encoded)

    elif n == 2:
        range = ORange.from_encoded(encoded)

    return range

class Key(object):
    def __init__(self, obj, *args):
        self.obj = obj
        (self.L, self.O) = self.chk(obj)

    def chk(self, obj):
        L = None
        if isinstance(obj, LCRange) or isinstance(obj, LCORange):
            L = obj.get_start_line()
        O = None
        if isinstance(obj, ORange) or isinstance(obj, LCORange):
            O = obj.get_start_offset()
        return (L, O)

    def __lt__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L < L
        elif self.O and O:
            b = self.O < O
        return b

    def __gt__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L > L
        elif self.O and O:
            b = self.O > O
        return b

    def __eq__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L == L
        elif self.O and O:
            b = self.O == O
        return b

    def __le__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L <= L
        elif self.O and O:
            b = self.O <= O
        return b

    def __ge__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L >= L
        elif self.O and O:
            b = self.O >= O
        return b

    def __ne__(self, other):
        (L, O) = self.chk(other.obj)
        b = False
        if self.L and L:
            b = self.L != L
        elif self.O and O:
            b = self.O != O
        else:
            b = True
        return b

