include ../rules.$(shell uname)
CBC_INST = $(HOME)/built
CPPFLAGS += -I$(CBC_INST)/include/coin -I$(ASL_HOME)/build/solvers
CXXFLAGS += -O0 -g

all: cbc_port nlmod

cbc_port: cbc_port.o
	$(CXX) -o $@ $(LDFLAGS) $^ -L$(CBC_INST)/lib $(LIBS) 

nlmod: nlmod.o common.o 
	$(CXX) -o $@ $(LDFLAGS) $^ -L$(CBC_INST)/lib $(ASL_HOME)/build/solvers/util/libutil.a -lamplsolver

common.o: common.cc common.h

clean:
	rm -f *.o cbc_port nlmod
