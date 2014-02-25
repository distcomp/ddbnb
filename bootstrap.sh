#!/bin/bash
git clone https://github.com/ampl/ampl.git
cd ampl
patch -p 1 < ../ampl-$(uname).patch

mkdir -p build
cd build
cmake ..
make

echo
echo Please ignore make errors, everything is probably OK
echo
