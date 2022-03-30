#!/bin/sh
rev=$(git log --format=format:%h HEAD^! || echo unknown)
year=$(date "+%Y")
echo "let version=\"revision $rev\"" > version.ml
echo "let copyright=\"Copyright 2012-${year} Codinuum Software Lab <https://codinuum.com>\"" >> version.ml
