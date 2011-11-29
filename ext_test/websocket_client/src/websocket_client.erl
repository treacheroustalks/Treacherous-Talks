-module(websocket_client).

-include("include/websocket.hrl").

-export([connect/5]).

-spec connect(string(), integer(), string(), string(), atom()) -> {ok, pid()}.
connect(Host, Port, Path, Origin, CallbackMod) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary,
                                              {packet, 0},
                                              {active, false}]),
    ok = handshake(Sock, Host, Path, Origin),
    WSState = #ws_state{ vsn = 8,
                         sock = Sock,
                         frag_type = none,
                         endpoint = client
                       },
    Pid = spawn(websocket_common, loop, [CallbackMod, WSState, {none, <<>>}, basic]),
    ok = inet:setopts(Sock, [{packet, raw}, {active, once}]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    {ok, Pid}.


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec handshake(term(), string(), string(), string()) -> ok.
handshake(Sock, Host, Path, Origin) ->
    Format =
        "GET ~s HTTP/1.1\r\n"
        "Host: ~s\r\n"
        "Upgrade: websocket\r\n"
        "Connection: Upgrade\r\n"
        "Sec-WebSocket-Key: ~s\r\n"
        "Sec-WebSocket-Origin: ~s\r\n"
        "Sec-WebSocket-Version: 8\r\n"
        "\r\n",
    Key = base64:encode_to_string(crypto:rand_bytes(16)),
    Request = io_lib:format(Format, [Path, Host, Key, Origin]),
    ok = gen_tcp:send(Sock, list_to_binary(Request)),
    {ok, Response} = gen_tcp:recv(Sock, 0),
    Headers = parse_headers(Response),
    ExpectedAccept = websocket_common:hash_nonce(Key),
    {_, WSAccept} = lists:keyfind("Sec-WebSocket-Accept", 1, Headers),
    case WSAccept of
        ExpectedAccept ->
            ok;
        _ ->
            exit("Expected Sec-WebSocket-Accept of "
                 ++ ExpectedAccept ++ " but got " ++ WSAccept)
    end.

parse_headers(<<HeaderBlockBin/binary>>) ->
    parse_headers(binary_to_list(HeaderBlockBin));
parse_headers(HeaderBlock) ->
    Lines = string:tokens(HeaderBlock, "\r\n"),
    % Remove HTTP response line
    Headers = lists:sublist(Lines, 2,length(Lines)-1),
    Fun = fun(Line) ->
                  Colon = string:str(Line, ":"),
                  Key = string:sub_string(Line, 1, Colon-1),
                  ValueUnstripped = string:sub_string(Line, Colon+1),
                  Value = string:strip(ValueUnstripped, both),
                  {Key, Value}
          end,
    lists:map(Fun, Headers).
