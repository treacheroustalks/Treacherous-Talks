
.PHONY: test

all: get_deps clean compile


get_deps:
	rebar get-deps


clean:
	rebar clean


compile:
	rebar compile


test:
	rebar eunit skip_deps=true
