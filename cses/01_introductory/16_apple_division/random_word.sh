#!/bin/bash
set -e
#set -x

DICT=/usr/share/dict/words
N=$(wc -l $DICT | awk '{print $1}')
RAND=$(openssl rand -hex 3 | tr a-f A-F | bc --ibase=16)
LINE1=$(( 1+($RAND % $N) ))
sed -n ${LINE1}p $DICT
