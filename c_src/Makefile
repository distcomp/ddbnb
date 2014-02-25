include ../rules.$(shell uname)
CBC_INST ?= $(HOME)/built
CPPFLAGS += -I$(CBC_INST)/include/coin -I$(ASL_HOME)/build/solvers
CXXFLAGS += -O0 -g

CBC_LINK := $(shell env PKG_CONFIG_PATH=$(CBC_INST)/lib/pkgconfig pkg-config --libs cbc)

all: cbc_port nlmod

cbc_port: cbc_port.o
	$(CXX) -o $@ -Wl,-rpath,$(CBC_INST)/lib $^ $(CBC_LINK)

nlmod: nlmod.o common.o 
	$(CXX) -o $@ $(LDFLAGS) $^ $(LINK_ASL)

common.o: common.cc common.h

clean:
	rm -f *.o cbc_port nlmod
