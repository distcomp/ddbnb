ASL_HOME ?= ../ampl
ASL_LIBS := -L$(ASL_HOME)/build/solvers $(ASL_HOME)/build/solvers/util/libutil.a -lamplsolver
CPPFLAGS += -I$(ASL_HOME)/build/solvers -I$(ASL_HOME) -I$(ASL_HOME)/solvers

CBC_HOME ?= /usr/local
CBC_LIBS := $(shell env PKG_CONFIG_PATH=$(CBC_HOME)/lib/pkgconfig pkg-config --libs cbc)
CPPFLAGS += -I$(CBC_HOME)/include/coin

SCIP_HOME ?= /usr/local
OSTYPE := $(shell uname -s | tr '[:upper:]' '[:lower:]')
SCIP_LIBS := -L$(SCIP_HOME)/lib -lscipopt-3.1.0.$(OSTYPE).$(shell uname -m).gnu.opt \
	-lz -lgmp -lreadline -lpthread
CPPFLAGS += -I$(SCIP_HOME)/include

OPT := -O0 -g
CXXFLAGS += $(OPT)
CFLAGS += $(OPT) -std=c99

include ../rules.$(shell uname)

all: cbc_port scip_port nlmod

cbc_port: cbc_port.o ErlPortInterface.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(CBC_LIBS)

scip_port : scip_port.o reader_nl.o event_all.o ErlPortInterface.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(SCIP_LIBS) $(ASL_LIBS)

nlmod: nlmod.o common.o 
	$(CXX) -o $@ $(LDFLAGS) $^ $(ASL_LIBS)

common.o: common.cc common.h

clean:
	rm -f *.o cbc_port scip_port nlmod
