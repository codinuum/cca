#!/bin/bash

DISTDIR=$(dirname $0)

if [ $# -lt 1 ]; then
    echo "usage: `basename $0` DIR"
    exit 1
fi


OS=`uname -s`

CORE_COUNT=1

if [ $OS = 'Linux' ]; then
    TIME=/usr/bin/time
    SHUF=/usr/bin/shuf
    CORE_COUNT=`grep 'core id' /proc/cpuinfo | sort | uniq | wc -l`

elif [ $OS = 'Darwin' ]; then
    TIME=/opt/local/bin/gtime
    SHUF=/opt/local/bin/gshuf
    CORE_COUNT=`sysctl -n machdep.cpu.core_count`
fi

BASE=$(basename $1)

NPROCS=10 #$CORE_COUNT

DIR=$(dirname $1)/$BASE

LOG=$DIR/test_parser.log

IFS=$'\n'

FILES=$(find -L -E $DIR \( -not -type l -and -type f \) -regex '.+\.([hcHC]|cc|hh|cpp|hpp)' -print|$SHUF)

array=($FILES)

NFILES=${#array[*]}

TASK_SIZE=$(($NFILES / NPROCS))

if [ $NFILES -eq 0 ]; then
    echo "no source files found"
    exit 0
fi


echo "*** PARSER TEST ***"
date
echo "parsing \"$1\"..."
echo "$NFILES files found"
echo "processes: $NPROCS"
echo "task size: $TASK_SIZE"


resid=$(($NFILES-($TASK_SIZE*$NPROCS)))

SPC="      "

INDENT=$SPC

for ((i=0;i<$NPROCS;i++)); do
    INDENT=${INDENT}${SPC}
    (
        sublog=${LOG}.$i
        rm -f $sublog
        total=0
        base=$(($i*$TASK_SIZE))

        if [ $i -eq $((${NPROCS}-1)) ]; then
            sz=$(($TASK_SIZE+$resid))
        else
            sz=$TASK_SIZE
        fi

        echo "task$i: size=$sz log=\"$sublog\""

        for ((j=0;j<$sz;j++)); do
            f=${array[$(($base+$j))]}
            ls -sh "$f" >> $sublog
            $TIME -f "TIME: %E" $DISTDIR/parser.opt "$f" >> $sublog 2>&1 || echo "[ABORTED] $f" >> $sublog
            total=$((total + 1))
            progress=$(($total * 100 / $sz))
            printf "  %s%3d%%\r" $INDENT $progress
        done
    )&
done

wait 

rm -f $LOG

for ((i=0;i<$NPROCS;i++)); do
    cat ${LOG}.$i >> $LOG
done

TOTAL=$NFILES
ABORT=$(grep '\[ABORTED\]' $LOG | wc -l | tr -d " ")
SUCC=$(grep 'PARSED' $LOG | wc -l | tr -d " ")
FAIL=$(grep '\[ERROR\]' $LOG | wc -l | tr -d " ")
WARN=$(grep '\[WARNING\]' $LOG | wc -l | tr -d " ")
#AMB=$(grep 'ambiguous nodes' $LOG | wc -l | tr -d " ")
#OMP_ERR=$(grep 'OMP error nodes' $LOG | wc -l | tr -d " ")
PERC=$(($SUCC * 100 / $TOTAL))
RESULT=NG

if [ $PERC -eq 100 -a $FAIL = '0' ]; then
    RESULT=OK
fi

printf "\n"
printf "parsed files:       %d/%d (%3.2f%%)\n" $SUCC $TOTAL $PERC
printf "errors:             %d\n" $FAIL
printf "uncaught exception: %d\n" $(($ABORT - $FAIL))
printf "warnings:           %d\n" $WARN
#printf "ambiguous:          %d\n" $AMB
#printf "OMP error:          %d\n" $OMP_ERR
printf "log file:           \"%s\"\n" $LOG
printf "******************************************************* %s -----> %s\n" $BASE $RESULT
date
