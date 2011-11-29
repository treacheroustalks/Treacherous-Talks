-module(test_client).

%% API
-export([connect/1]).

%% Export for websocket_client
-export([handle_message/1]).

%%------------------------------------------------------------------------------
%% Type :: text|binary
%% Data :: binary()
%% HandlerResult :: {reply, {Type, Data}}
%%                | {noreply}
%%                | {close, Reason}
%% handle_message({Type, Data}) -> HandlerResult
%%------------------------------------------------------------------------------
handle_message({text, Data}) ->
    io:format("server said ~p~n", [Data]),
    {noreply}.

connect(wsorg) ->
    Host = "echo.websocket.org",
    Port = 80,
    Path = "/",
    Origin = "http://websocket.org",
    websocket_client:connect(Host, Port, Path, Origin, ?MODULE);
connect(jbothma) ->
    Host = "jbothma.co.uk",
    Port = 8000,
    Path = "/websockets_example_endpoint.yaws",
    Origin = "http://jbothma.co.uk",
    websocket_client:connect(Host, Port, Path, Origin, ?MODULE);
connect(localtt) ->
    Host = "127.0.0.1",
    Port = 8000,
    Path = "/endpoint",
    Origin = "",
    websocket_client:connect(Host, Port, Path, Origin, ?MODULE).
