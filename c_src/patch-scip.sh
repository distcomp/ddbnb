#!/bin/bash
set -e
if [ $# -ne 1 ]; then
    echo "Usage: $0 <scipoptsuite path>"
    exit
fi
DIR=$(dirname $0)
patch -d $1/ug -p1 < $DIR/parascip-ddbnb.patch
cp -v $DIR/ErlPortInterface.cc $1/ug/src/ug/ErlPortInterface.cpp
cp -v $DIR/ErlPortInterface.h $1/ug/src/ug
