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

echo 'Downloading everest.py...'
cd ..
curl https://gitlab.com/everest/python-api/raw/master/everest.py > everest/everest.py

echo
echo Done!
echo
