REBAR=./rebar

.PHONY: all compile clean test

all: compile

compile: 
	${REBAR} compile

clean:
	${REBAR} clean
	rm -fv erl_crash.dump

test: compile
	${REBAR} eunit skip_deps=true

