all: deps compile docs

docs:
	rebar skip_deps=true doc

deps:
	rebar get-deps

small_clean:
	rebar skip_deps=true clean

clean:
	rebar clean

compile:
	rm -rf .eunit
	rebar compile

test:
	rebar skip_deps=true eunit

dia:
	dialyzer -Wno_return -I ../ ebin

complete: small_clean deps compile dia test docs
	@echo ok

.PHONY: all small_clean clean compile test docs dia complete
