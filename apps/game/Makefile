all: deps compile docs

docs: test
	rebar skip_deps=true doc
	rm -rf doc/coverage
	mkdir doc/coverage
	cp -r .eunit/*.html doc/coverage

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
	dialyzer -Wno_return -I ../ --src src

complete: small_clean deps compile dia test docs
	@echo ok

.PHONY: all small_clean clean compile test docs dia complete
