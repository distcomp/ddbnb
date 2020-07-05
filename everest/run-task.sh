#!/bin/bash
export OPENMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
chmod +x $1 2>/dev/null
cat /proc/meminfo
if [[ "$1" == *"parascip"* ]]; then
    PART=""
    if [[ "$SLURM_DEF_PART" != "" ]]; then
        PART="-p $SLURM_DEF_PART"
    fi
    ulimit -c 0
    mkdir -p logs
    salloc $PART -n 16 -t 4:00:00 mpirun python -u task.py $*
    RET=$?
    tar cjf logs.tbz logs
else
    python -u task.py $*
    RET=$?
    touch logs.tbz
fi
tar czf stdout.tgz stdout
exit $RET
