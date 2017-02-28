#!/bin/bash

if [ $# -lt 1 ]; then
    echo "usage: `basename $0` DIR"
    exit 0
fi


DIR=$1

for f in `find $DIR -name '*.py' -print`
do
  ls -sh $f; ./parser.opt $f
done
