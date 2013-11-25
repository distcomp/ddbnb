#!/bin/bash
TMP_SOL=.---solve---.erl
ADDR=$(ifconfig | grep inet | sed -e /inet6/d -e /127.0.0.1/d | head -n 1 | sed 's/[^0-9]*\([0-9.][0-9.]*\).*$/\1/')
cat $(dirname $0)/solve.erl | sed "s/%%%NAME%%%/-name master$$@$ADDR/" > $TMP_SOL
ERL_LIBS=$(dirname $0)/.. escript $TMP_SOL $*
rm -f $TMP_SOL
