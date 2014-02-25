#!/bin/bash
if [[ -z "$1" ]]; then
    echo "Usage: $0 <node name>"
    exit 1
fi
erl -noshell -name temp_control \
    -eval "io:format(\"Stopping node ~p: ~p~n\", ['$1', rpc:call('$1', init, stop, [])])" \
    -s init stop
