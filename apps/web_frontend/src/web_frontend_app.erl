-module(web_frontend_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format ("starting [~p]~n", [?MODULE]),
    pg2:start_link (),
    case net_adm:ping('backend@127.0.0.1') of
        pang ->
            erlang:error ({error, could_not_ping_hardcoded_backend});
        pong -> ok
    end,
    web_frontend_sup:start_link().

stop(_State) ->
    ok.
