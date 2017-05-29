#!/bin/bash
R=$(dirname $0)

NCPU=$1
if [[ $1 -eq "" ]]; then
    NCPU=0
fi

PARAM="-detached"
if [[ "$2" == "debug" ]]; then
    PARAM="-noinput"
fi

erl -pa $R/ebin -name slave@$($R/my_ip.sh) -boot start_sasl -s dcbc_app -config $R/slave.config \
    -dcbc cbc_path  "\"$R/../c_src/cbc_port\"" -dcbc num_cpu $NCPU -dcbc registry_node "'$(cat registry-node)'" $PARAM
