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

echo
echo Done!
echo
