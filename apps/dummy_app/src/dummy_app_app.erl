-module(dummy_app_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("starting supervisor~n"),
    dummy_app_server:start_link(),
    dummy_app_sup:start_link().

stop(_State) ->
    ok.
