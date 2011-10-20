

all: get_deps clean compile docs


get_deps:
	rebar get-deps


clean:
	rebar clean; rm -rf doc/


compile:
	rebar compile


tests: test/*
	rebar eunit skip_deps=true

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}]'
