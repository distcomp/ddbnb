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
R=$(dirname $0)

erl -pa $R/ebin -name master@$($R/my_ip.sh) -boot start_sasl -s dcbc_app -dcbc working_mode master \
    -dcbc files "$FILES" -dcbc args "$ARGS"  -dcbc registry_node `cat $R/registry-node`
