# DCBC for Everest platform

## Requirements

For computing node

  * Everest agent (see https://gitlab.com/everest/agent/blob/master/docs/manual.adoc)
  * SCIP >= 3.2.1 and/or CBC >= 2.9
  * cmake
  * GNU Make
  * GCC
  * Git

For user

  * Python 2.7
  * Git

## Installing on computing node

Here we build and install special adapters to CBC and SCIP solvers to /usr/local/bin. CBC and/or SCIP are assumed to be installed in /usr/local too.

1. Clone the git project:
  ```
  git clone https://github.com/ssmir/dcbc.git
  ```
2. Build scip_port and cbc_port
  * Copy reader_nl.c and reader_nl.h from interfaces/ampl/src directory of SCIP sources to c_src
  * From the project's root directory:
  ```
  ./bootstrap.sh
  make
  sudo make install
  ```
3. Check agent configuration:
  * taskProtocol.enabled = true
  * security.allowedCommands should contain "bash run-task.sh .*"

## Usage

1. Clone the git repository:
  ```
  git clone https://github.com/ssmir/dcbc.git
  ```
2. Prepare subproblems and run:
  ```
  dcbc/everest/batch_solve.py stub1.nl stub2.nl
  ```
3. To get help use dcbc/everest/batch_solve.py -h
