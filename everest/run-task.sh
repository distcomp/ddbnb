#!/bin/bash
export OPENMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
python -u task.py $*
