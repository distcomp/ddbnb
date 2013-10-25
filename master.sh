#!/bin/bash
if [ "$#" -eq 0 ]; then
    echo "Usage: $0 [solver args --] <Stub files>"
    exit 1
fi

ARGS=""
FILES=""
while [ $# -gt 0 ]; do
    case "$1" in
        "--")
            shift
            ARGS="$FILES"
            FILES=""
            ;;
        *)
            FILES="$FILES \"$1\""
            shift
            ;;
    esac
done


FILES=[$(echo $FILES | sed 's/ /, /g')]
ARGS=[$(echo $ARGS | sed 's/ /, /g')]

ADDR=$(ifconfig | grep inet | sed -e /inet6/d -e /127.0.0.1/d | head -n 1 | sed 's/[^0-9]*\([0-9.][0-9.]*\).*$/\1/')

erl -pa $(dirname $0)/ebin -name master@$ADDR -boot start_sasl -s dcbc_app -dcbc working_mode master \
    -dcbc files "$FILES" -dcbc args "$ARGS"  -dcbc registry_node `cat $(dirname $0)/registry-node`
