
.PHONY: all get_deps clean compile test doc

all: get_deps clean compile doc


get_deps:
	rebar get-deps


clean:
	rebar clean


compile:
	rebar compile


doc:
	rebar skip_deps=true doc


test:
	rebar eunit skip_deps=true
