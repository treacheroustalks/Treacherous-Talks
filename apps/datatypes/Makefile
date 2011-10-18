

all: get_deps clean compile tests docs


get_deps:
	rebar get-deps


clean:
	rebar clean


compile:
	rebar compile


tests:
	rebar eunit

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}]'