

all: get_deps compile doc


get_deps:
	rebar get-deps


clean:
	rebar clean


compile: src
	rebar compile


doc: compile
	rebar skip_deps=true doc


tests:
	rebar eunit skip_deps=true
