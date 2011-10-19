all: deps compile doc

doc:
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

.PHONY: all clean compile test
