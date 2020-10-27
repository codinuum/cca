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


URI_BASE = 'http://codinuum.com/fact/'

SPEC_NS      = URI_BASE + 'spec#'
ENTITY_NS    = URI_BASE + 'entity/'
PREDICATE_NS = URI_BASE + 'predicate#'
VERSION_NS   = URI_BASE + 'version/'
RELEASE_NS   = VERSION_NS + 'release/'
SVNREV_NS    = VERSION_NS + 'svn/revision/'
GITREV_NS    = VERSION_NS + 'git/revision/'
VARIANT_NS   = VERSION_NS + 'variant/'
EXTERNAL_NS  = URI_BASE + 'external/'
GUARD_NS     = URI_BASE + 'guard/'

SEP = '-'
SUB_SEP = '_'
SUB_SUB_SEP = '.'
