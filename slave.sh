#!/bin/bash
ADDR=$(ifconfig | grep inet | sed -e /inet6/d -e /127.0.0.1/d | head -n 1 | sed 's/[^0-9]*\([0-9.][0-9.]*\).*$/\1/')

NCPU=$1
if [[ $1 -eq "" ]]; then
    NCPU=0
fi
cat slave.config.skel | sed "s/%%%REGISTRY%%%/$(cat registry-node)/" > slave.config
erl -pa ebin -detached -name slave@$ADDR -boot start_sasl -s dcbc_app -config slave.config \
    -dcbc cbc_path  "\"$PWD/c_src/cbc_port\"" -dcbc num_cpu $NCPU
