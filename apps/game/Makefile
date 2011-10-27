all: deps compile docs

docs:
	rebar skip_deps=true doc

deps:
	rebar get-deps

clean:
	rebar clean

compile:
	rm -rf .eunit
	rebar compile

test:
	rebar skip_deps=true eunit

dia: 
	dialyzer -Wno_return -I ../ ebin

.PHONY: all clean compile test docs
