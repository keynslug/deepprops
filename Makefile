REBAR=./rebar

.PHONY: all compile clean test

all: compile

compile:
	${REBAR} get-deps compile

clean:
	${REBAR} clean
	rm -fv erl_crash.dump

test: compile
	${REBAR} -C rebar.tests.config get-deps compile
	${REBAR} -C rebar.tests.config eunit skip_deps=true

