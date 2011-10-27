all: deps clean compile docs


deps:
	rebar get-deps


clean:
	rebar clean


compile:
	rebar compile


docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}]'
test:
	rebar eunit

.PHONY: all deps clean compile test docs
