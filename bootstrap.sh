#!/bin/bash

if ! [ -a ampl ]; then
  git clone https://github.com/ampl/ampl.git
fi
cd ampl
git checkout 7579113
patch -N -p 1 < ../ampl.patch

rm -rf build
mkdir -p build
cd build
cmake ..
make asl
cd ../..

if [[ -f /usr/local/src/reader_nl.tgz ]]; then
    echo 'Found reader_nl.tgz, extracting...'
    cd c_src
    tar xzf /usr/local/src/reader_nl.tgz
fi

echo
echo Done!
echo
