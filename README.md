# Branch-and-bound with domain decomposition (DDBNB)

Solves a MILP problem with a distributed set of B&B solver solver instances.
Compatible solvers:
  * CBC, https://projects.coin-or.org/Cbc)
  * SCIP, http://scip.zib.de
  
## Implementation for Everest platform

### Requirements

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

### Installing on computing node

Here we build and install special adapters to CBC and SCIP solvers to /usr/local/bin. CBC and/or SCIP are assumed to be installed in /usr/local too.

1. Clone the git project:
  ```
  git clone https://github.com/distcomp/ddbnb.git
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

### Usage

1. Clone the git repository:
  ```
  git clone https://github.com/distcomp/ddbnb.git
  ```
2. Make a token for ddbnb:
  ```
  python ddbnb/everest/everest.py get-token -u <EVEREST_LOGIN> -l ddbnb-XXX > ~/.everest_token
  ```
3. Prepare subproblems and run:
  ```
  ddbnb/everest/batch_solve.py stub1.nl stub2.nl
  ```
4. To get help use ddbnb/everest/batch_solve.py -h


## External dependencies

* AMPL solver library (http://www.netlib.org/ampl/solvers/ or https://github.com/ampl/ampl)
* ASL util library by Victor Zverovich (https://github.com/ampl/ampl)
* CBC (https://projects.coin-or.org/Cbc)
* SCIP (http://scip.zib.de/)
