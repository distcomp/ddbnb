#!/bin/bash

if ! [ -a ampl ]; then
  git clone https://github.com/ampl/ampl.git
fi
cd ampl
patch -N -p 1 < ../ampl.patch

rm -rf build
mkdir -p build
cd build
cmake ..
make

echo
echo Done!
echo
