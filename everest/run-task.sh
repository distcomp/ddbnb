#!/bin/bash
export OPENMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
chmod +x $1 2>/dev/null
python -u task.py $*
RET=$?
tar czf stdout.tgz stdout
exit $RET
