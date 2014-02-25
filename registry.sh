#!/bin/bash
R=$(dirname $0)

PARAM="-detached"
if [[ "$1" == "debug" ]]; then
    PARAM="-noinput"
fi

erl -pa $R/ebin -name `cat registry-node` -boot start_sasl -s dcbc_app \
    -config $R/registry.config $PARAM
