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
	rm -rf doc/coverage
	mkdir doc/coverage
	cp -r .eunit/*.html doc/coverage

dia:
	dialyzer -Wno_return -I ../ --src src

complete: small_clean deps compile test docs dia
	@echo ok

.PHONY: all small_clean clean compile test docs dia complete
