#!/bin/bash
if [[ -z "$1" ]]; then
    echo "Usage: $0 <node name>"
    exit 1
fi
R=$(dirname $0)
erl -noshell -name temp_control@$($R/my_ip.sh) \
    -eval "io:format(\"Stopping node ~p: ~p~n\", ['$1', rpc:call('$1', init, stop, [])])" \
    -s init stop
