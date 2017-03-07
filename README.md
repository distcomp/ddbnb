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

### Usage

1. Clone the git repository:
  ```
  git clone https://github.com/ssmir/dcbc.git
  ```
2. Make a token for dcbc:
  ```
  python dcbc/everest/everest.py get-token -u <EVEREST_LOGIN> -l dcbc-XXX > ~/.everest_token
  ```
3. Prepare subproblems and run:
  ```
  dcbc/everest/batch_solve.py stub1.nl stub2.nl
  ```
4. To get help use dcbc/everest/batch_solve.py -h


## Erlang Implementation

### Executables

```
c_src/cbc_port
```

A port program for communicating with CBC from Erlang.

```
c_src/scip_port
```
A port program for communicating with SCIP from Erlang.

```
c_src/nlmod
```

A program for splitting a problem, encoded as AMPL stub, to a set of subproblems.

```
registry.sh
```

Starts an Erlang node managing registration of slave nodes. Only one registry
node should be started. Its node@host should be set in the registry-node file.

```
slave.sh [number of cpu]
```

Starts an Erlang node which starts B&B solver instances. It should be started
one per host.

```
master.sh [solver options <-->] <list of .nl files of subproblems>
```
Solve a set of subproblems on all available slave nodes found in registry.

```
solve.sh [solver options <-->] <list of .nl files of subproblems>
```
Same as master.sh but exits after solving the last subproblem.


### Installation

1. Install CBC and Erlang.
  https://www.erlang-solutions.com/downloads/download-erlang-otp

2. On the command line:
  ```
  git clone https://github.com/ssmir/dcbc.git
  cd dcbc
  ./bootstrap.sh
  make
  ```
  If you wish to use SCIP:
  Copy reader_nl.h and reader_nl.c from interfaces/ampl/src in SCIP source tree
  to dcbc/c_src. Then, if SCIP libraries are installed in /usr/local/lib and
  SCIP headers are in /usr/local/include:
  `make USE_SCIP=true SCIP_HOME=/usr/local`

  If it doesn't compile, also add `SCIP_LIBS=<libraries needed to link with SCIP>`
  to make command line

3. Erlang cookie file ~/.erlang.cookie have to be same on every node working
  with DCBC

### Usage

1. Edit registry-node file. It has to contain the name of the registry node with
an IP address reachable from other nodes, for example:
```
registry@192.168.0.35
```

2. Start the registry node on the machine with the address specified
in the registry-node file:
```
./registry.sh
```

3. Start slave nodes on every machine used for computations.
```
./slave.sh
```

It's possible to specify the maximum number of solver processes running
simultaneously:
`./slave 5`
would run maximum 5 processes

4. Solve some problems with solve.sh:
```
./solve.sh 1.nl 2.nl 3.nl
```


Shutting down:

It's possible to shut down a node running on the local machine:
```
./stop-node.sh slave
./stop-node.sh registry
```

## External dependencies

AMPL solver library (http://www.netlib.org/ampl/solvers/ or https://github.com/ampl/ampl)
ASL util library by Victor Zverovich (https://github.com/ampl/ampl)
CBC (https://projects.coin-or.org/Cbc)
SCIP (http://scip.zib.de/)
Erlang (http://www.erlang.org)
