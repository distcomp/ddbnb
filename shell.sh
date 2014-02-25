#!/bin/bash
ADDR=$(/sbin/ifconfig | grep inet | sed -e /inet6/d -e /127.0.0.1/d | head -n 1 | sed 's/[^0-9]*\([0-9.][0-9.]*\).*$/\1/')

erl -name shell@$ADDR -remsh $1@$ADDR
