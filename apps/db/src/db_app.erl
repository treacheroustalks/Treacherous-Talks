-module(db_app).
-vsn("1.0.0").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("[~p] starting~n", [?MODULE]),
    db_sup:start_link().

stop(_State) ->
    ok.
