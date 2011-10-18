

all: get_deps clean compile docs


get_deps:
	rebar get-deps


clean:
	rebar clean


compile:
	rebar compile


tests: test/*
	rebar eunit

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}]'
