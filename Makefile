CBC_PORT = c_src/cbc_port

compile: c_src
	./rebar compile

c_src:
	$(MAKE) -C c_src

deps:
	./rebar get-deps
	./rebar update-deps

test: compile
	cd test && ./run_test.sh

clean:
	$(MAKE) -C c_src clean
	./rebar clean
	rm -f erl_crash.dump

.PHONY: deps test clean compile c_src
