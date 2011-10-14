-module(db_user_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("starting supervisor~n"),
    db_user_server:start_link(),
    db_user_sup:start_link().

stop(_State) ->
    ok.
