#!/bin/bash
echo "Preparing problem..."
../c_src/nlmod BalanceTestDyn.nl split 120

echo "Starting registry..."
../registry.sh debug &
sleep 1

echo "Starting slave..."
../slave.sh 2 debug &
sleep 1

echo "Solving the problem..."
../solve.sh BalanceTestDyn_*.nl

../stop-node.sh $(cat registry-node)
../stop-node.sh slave@$(../my_ip.sh)
