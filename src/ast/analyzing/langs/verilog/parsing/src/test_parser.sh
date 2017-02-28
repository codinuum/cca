#!/bin/bash

DISTDIR=$(dirname $0)

if [ $# -lt 1 ]; then
    echo "usage: `basename $0` DIR"
    exit 1
fi

BASE=$(basename $1)

DIR=$(dirname $1)/$BASE

LOG=$DIR/test_parser.log

IFS=$'\n'

FILES=$(find -L -E $DIR \( -not -type l -and -type f \) -regex '.+\.v' -print)

array=($FILES)

NFILES=${#array[*]}

if [ $NFILES -eq 0 ]; then
    echo "no source files found"
    exit 0
fi

rm -f $LOG

TOTAL=0

echo "*** PARSER TEST ***"
date
echo "parsing \"$1\"..."
echo "$NFILES files found"

PREV=0

for f in ${array[@]}; do
  ls -sh "$f" >> $LOG
  $DISTDIR/parser.opt "$f" >> $LOG 2> /dev/null
  TOTAL=$((TOTAL + 1))
  PROGRESS=$(($TOTAL * 100 / $NFILES))
  if [ $PREV -ne $PROGRESS ]; then
      PREV=$PROGRESS
      printf "  %d%%\r" $PROGRESS
  fi
done


SUCC=$(grep PARSED $LOG | wc -l | tr -d " ")
FAIL=$(grep '\[ERROR\]' $LOG | wc -l | tr -d " ")
WARN=$(grep '\[WARNING\]' $LOG | wc -l | tr -d " ")
AMB=$(grep 'ambiguous nodes' $LOG | wc -l | tr -d " ")
MISSED=$(grep 'missed LOC' $LOG | wc -l | tr -d " ")
PERC=$(($SUCC * 100 / $TOTAL))

if [ $PERC -eq 100 ]; then
    RESULT=OK
else
    RESULT=NG
fi

printf "parsed files:       %d/%d (%3.2f%%)\n" $SUCC $TOTAL $PERC
printf "errors:             %d\n" $FAIL
printf "uncaught exception: %d\n" $(($TOTAL - $SUCC - $FAIL))
printf "warnings:           %d\n" $WARN
printf "ambiguous:          %d\n" $AMB
printf "missed:             %d\n" $MISSED
printf "log file:           \"%s\"\n" $LOG
printf "******************************************************* %s -----> %s\n" $BASE $RESULT
date
