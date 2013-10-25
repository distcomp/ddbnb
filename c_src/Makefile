CBC_INST = ../../built
CPPFLAGS = -I$(CBC_INST)/include/coin #-DNO_PORT
CXXFLAGS = -O0 -g

all: cbc_port

cbc_port: cbc_port.o
	$(CXX) -o $@ $^ -L$(CBC_INST)/lib -lCbcSolver -lcoinasl -lCbc -lCgl -lOsiClp -lOsi -lCoinUtils -lClp -lcoinlapack -lcoinblas

clean:
	rm -f *.o cbc_port
