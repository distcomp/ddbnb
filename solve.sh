#!/bin/bash
R=$(dirname $0)
TMP_SOL=.---solve---.erl
cat $R/solve.erl | sed -e "s/%%%NAME%%%/-name master$$@$($R/my_ip.sh)/" \
    -e "s/%%%REGISTRY%%%/$(cat registry-node)/" > $TMP_SOL
ERL_LIBS=$R/.. escript $TMP_SOL $*
rm -f $TMP_SOL
