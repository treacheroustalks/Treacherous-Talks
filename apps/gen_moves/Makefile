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
	$(REBAR) compile

test:
	$(REBAR) skip_deps=true eunit
	rm -rf doc/coverage
	mkdir doc/coverage
	cp -r .eunit/*.html doc/coverage

gen_moves.plt: compile
	dialyzer -q --build_plt --output_plt gen_moves.plt ebin

dia: ebin gen_moves.plt test
	dialyzer -q -I .. --plts gen_moves.plt ~/.dialyzer_plt ../game/game.plt ../utils/utils.plt -- ebin

complete: small_clean deps compile test docs
	@echo ok

.PHONY: all small_clean clean compile test docs dia complete
