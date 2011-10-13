-module(db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("starting supervisor~n"),
    db_server:start_link(),
    db_sup:start_link().

stop(_State) ->
    ok.
