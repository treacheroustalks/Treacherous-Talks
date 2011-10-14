-module(ejabberd_echo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("starting supervisor~n"),
    ejabberd_echo_server:start_link(),
    ejabberd_echo_sup:start_link().

stop(_State) ->
    ok.
