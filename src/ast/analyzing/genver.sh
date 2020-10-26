#!/bin/sh
rev=$(git log --format=format:%h HEAD^! || echo unknown)
echo "let version=\"revision $rev\"" > version.ml
echo "let copyright=\"Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>\"" >> version.ml
