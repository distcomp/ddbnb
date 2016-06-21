ASL_HOME ?= ../ampl
ASL_LIBS := -L$(ASL_HOME)/build/lib -lasl -lmp
CPPFLAGS += -I$(ASL_HOME)/include -I$(ASL_HOME)/src/asl/include -I$(ASL_HOME)/src/asl/solvers -I$(ASL_HOME)/build/src/asl -I$(ASL_HOME)/src

CBC_HOME ?= /usr/local
CBC_LIBS := $(shell env PKG_CONFIG_PATH=$(CBC_HOME)/lib/pkgconfig pkg-config --libs cbc)
CPPFLAGS += -I$(CBC_HOME)/include/coin

SCIP_HOME ?= /usr/local
SCIP_LIBS ?= -L$(SCIP_HOME)/lib -lscipopt -lipopt -lz -lgmp -lreadline -lpthread
CPPFLAGS += -I$(SCIP_HOME)/include

OPT := -O0 -g
CXXFLAGS += $(OPT)
CFLAGS += $(OPT)

include ../rules.$(shell uname)

TARGETS := nlmod
TARGETS += $(shell if scip -q -c quit; then echo scip_port; fi)
TARGETS += $(shell if cbc quit > /dev/null; then echo cbc_port; fi)

all: $(TARGETS)

cbc_port: cbc_port.o ErlPortInterface.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(CBC_LIBS)

scip_port : scip_port.o reader_nl.o event_all.o ErlPortInterface.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(SCIP_LIBS) $(ASL_LIBS)

nlmod: nlmod.o common.o 
	$(CXX) -o $@ $(LDFLAGS) $^ $(ASL_LIBS)

common.o: common.cc common.h

clean:
	rm -f *.o cbc_port scip_port nlmod
