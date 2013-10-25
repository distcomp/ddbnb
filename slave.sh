#!/bin/bash
REG="'registry@localhost'"
if [ -nz "$1" ]; then
    REG="'registry@$1'"
fi
erl -pa ebin -name slave$1 -boot start_sasl -s dcbc_app -dcbc working_mode slave \
    -dcbc cbc_path  "\"$PWD/c_src/cbc_port\"" -dcbc registry_node `cat registry-node`
