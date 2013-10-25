include ../rules.$(shell uname)
CBC_INST = $(HOME)/built
CPPFLAGS = -I$(CBC_INST)/include/coin #-DNO_PORT
CXXFLAGS = -O0 -g
LIBS := -lCbcSolver -lcoinasl -lCbc -lCgl -lOsiClp -lOsi -lCoinUtils -lClp $(LIBS)

all: cbc_port

cbc_port: cbc_port.o
	$(CXX) -o $@ $(LDFLAGS) $^ -L$(CBC_INST)/lib $(LIBS) 

clean:
	rm -f *.o cbc_port
