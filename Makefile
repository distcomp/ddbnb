CBC_PORT = c_src/cbc_port

compile: $(CBC_PORT)
	./rebar compile

$(CBC_PORT) : c_src/cbc_port.cc
	$(MAKE) -C c_src

deps:
	./rebar get-deps
	./rebar update-deps

clean:
	$(MAKE) -C c_src clean
	./rebar clean
	rm -f erl_crash.dump

.PHONY: deps test clean compile
