# Generic app Makefile for Treacherous Talks

REBAR=../../rebar

all: small_clean deps compile docs

docs:
	$(REBAR) skip_deps=true doc

deps:
	$(REBAR) get-deps

small_clean:
	$(REBAR) skip_deps=true clean

clean:
	$(REBAR) clean

compile:
	rm -rf .eunit
	$(REBAR) compile

test:
	$(REBAR) skip_deps=true eunit

dia:
	dialyzer -Wno_return -I ../ --src src

complete: small_clean deps compile test docs dia
	@echo ok

.PHONY: all small_clean clean compile test docs dia complete
