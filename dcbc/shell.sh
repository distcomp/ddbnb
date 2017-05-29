#!/bin/bash
ADDR=$($(dirname $0)/my_ip.sh)

NODE=$(cat registry-node)
if [[ "$1" != "registry" ]]; then
    NODE="$1@$ADDR"
fi

erl -name shell@$ADDR -remsh $NODE
