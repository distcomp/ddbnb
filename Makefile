CBC_PORT = c_src/cbc_port

compile: c_src
	cd dcbc && ./rebar compile

c_src:
	$(MAKE) -C c_src

deps:
	cd dcbc && ./rebar get-deps
	cd dcbc && ./rebar update-deps

test: compile
	$(MAKE) -C test

install: c_src
	cp -f c_src/cbc_port c_src/scip_port /usr/local/bin

clean:
	$(MAKE) -C test clean
	$(MAKE) -C c_src clean
	cd dcbc && ./rebar clean
	rm -f erl_crash.dump *.log solution.sol

.PHONY: deps test clean compile c_src
