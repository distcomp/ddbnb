#!/bin/bash
echo "Preparing problem..."
../c_src/nlmod BalanceTestDyn.nl split 120

echo "Starting registry..."
../dcbc/registry.sh debug &
sleep 1

echo "Starting slave..."
../dcbc/slave.sh 2 debug &
sleep 1

echo "Solving the problem..."
../dcbc/solve.sh BalanceTestDyn_*.nl

../dcbc/stop-node.sh $(cat registry-node)
../dcbc/stop-node.sh slave@$(../dcbc/my_ip.sh)
