-module(websocket_common).

-include("websocket.hrl").

-define(MAX_PAYLOAD, 16777216). %16MB

%% API
-export([hash_nonce/1, loop/4, send/2]).


send(#ws_state{sock=Socket,
               vsn=ProtoVsn,
               endpoint=EndpointType}, {FrameType, Data}) ->
    DataFrame = frame(ProtoVsn, FrameType,  Data, EndpointType),
    case Socket of
        {sslsocket,_,_} ->
            ssl:send(Socket, DataFrame);
        _ ->
            gen_tcp:send(Socket, DataFrame)
    end;

send(Pid, {Type, Data}) ->
    Pid ! {send, {Type, Data}},
    ok.


loop(CallbackMod, WSState = #ws_state{sock=Socket}, CallbackState, CallbackType) ->
    receive
        {send, {Type, Data}} ->
            send(WSState, {Type, Data}),
            loop(CallbackMod, WSState, CallbackState, CallbackType);
        {tcp, Socket, FirstPacket} ->
            FrameInfos = unframe_active_once(WSState, FirstPacket),
            case CallbackType of
                basic ->
                    {BasicMessages, NewCallbackState} =
                        basic_messages(FrameInfos, CallbackState),
                    CallbackResults = lists:map({CallbackMod, handle_message}, BasicMessages),
                    lists:map(handle_result_fun(WSState), CallbackResults);
                {advanced,_} ->
                    NewCallbackState = lists:foldl(do_callback_fun(WSState, CallbackMod), 
                                                   CallbackState,
                                                   FrameInfos)
            end,
            Last = lists:last(FrameInfos),
            NewWSState = Last#ws_frame_info.ws_state,

            loop(CallbackMod, NewWSState, NewCallbackState, CallbackType);
        {tcp_closed, Socket} ->
            io:format("Websocket closed. Terminating echo_server...~n");
        Any ->
            io:format("websocket server loop received msg:~p~n", [Any]),
            loop(CallbackMod, WSState, CallbackState, CallbackType)
    end.

handle_result_fun(WSState) ->
    fun(Result) ->
            case Result of
                {reply, {Type, Data}} ->
                    send(WSState, {Type, Data});
                {noreply} ->
                    ok;
                {close, Reason} ->
                    exit(Reason)

            end
    end.

do_callback_fun(WSState, CallbackMod) ->
    fun(FrameInfo, CallbackState) ->
            case CallbackMod:handle_message(FrameInfo, CallbackState) of
                {reply, {Type, Data}, NewCallbackState} ->
                    send(WSState, {Type, Data}),
                    NewCallbackState;
                {noreply, NewCallbackState} ->
                    NewCallbackState;
                {close, Reason} ->
                    exit(Reason)
            end
    end.

basic_messages(FrameInfos, {FragType, FragAcc}) ->
    {Messages, NewFragType, NewFragAcc}
        = lists:foldl(fun handle_message/2, {[], FragType, FragAcc}, FrameInfos),
    {Messages, {NewFragType, NewFragAcc}}.

%% start of a fragmented message
handle_message( #ws_frame_info{ fin=0,
                                opcode=FragType,
                                data=Data },
                {Messages, none, <<>>}) ->
    {Messages, FragType, Data};

%% non-final continuation of a fragmented message
handle_message( #ws_frame_info{ fin=0,
                                data=Data,
                                opcode=continuation},
                {Messages, FragType, FragAcc}) ->
    {Messages, FragType, <<FragAcc/binary,Data/binary>>};

%% end of text fragmented message
handle_message( #ws_frame_info{ fin=1,
                                opcode=continuation,
                                data=Data
                              },
                {Messages, text, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {text, Unfragged},
    {[NewMessage | Messages], none, <<>>};

%% unfragmented text message
handle_message( #ws_frame_info{opcode=text, data=Data},
                {Messages, none, <<>>}) ->
    NewMessage = {text, Data},
    {[NewMessage | Messages], none, <<>>};

%% end of binary fragmented message
handle_message( #ws_frame_info{ fin=1,
                                opcode=continuation,
                                data=Data
                              },
                {Messages, binary, FragAcc}) ->
    Unfragged = <<FragAcc/binary, Data/binary>>,
    NewMessage = {binary, Unfragged},
    {[NewMessage|Messages], none, <<>>};

handle_message( #ws_frame_info{ opcode=binary,
                                data=Data
                              },
                {Messages, none, <<>>}) ->
    NewMessage = {binary, Data},
    {[NewMessage|Messages], none, <<>>};

handle_message( #ws_frame_info{ opcode=ping,
                                data=Data,
                                ws_state=State},
                Acc) ->
    io:format("Replying pong to ping on behalf of basic callback module.~n",[]),
    send(State, {pong, Data}),
    Acc;

handle_message(#ws_frame_info{opcode=pong}, Acc) ->
    % A response to an unsolicited pong frame is not expected.
    % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4
    io:format("ignoring unsolicited pong~n",[]),
    Acc;

handle_message(FrameInfo=#ws_frame_info{}, Acc) ->
    io:format("WS Endpoint Ignoring message for basic callback. ~p~n~p~n",
              [FrameInfo, Acc]),
    Acc.

buffer(Socket, Len, Buffered) ->
    case Buffered of
        <<_Expected:Len/binary>> = Return -> % exactly enough
%            debug(val, {buffering, "got:", Len}),
            Return;
        <<_Expected:Len/binary,_Extra/binary>> = Return-> % more than expected
%            debug(val, {buffering, "got:", Len, "and more!"}),
            Return;
        _ -> % not enough
%            debug(val, {buffering, "need:", Len, "waiting for more..."}),
            % TODO: take care of ssl sockets
            Needed = Len - binary_length(Buffered),
            {ok, More} = gen_tcp:recv(Socket, Needed),
            <<Buffered/binary, More/binary>>
    end.

binary_length(<<>>) ->
    0;
binary_length(<<_First:1/binary, Rest/binary>>) ->
    1 + binary_length(Rest).


checks(Unframed) ->
    check_reserved_bits(Unframed).

check_control_frame(Len, Opcode, Fin) ->
    if
        (Len > 125) and (Opcode > 7) ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4.5
            {fail_connection, "control frame > 125 bytes"};
        (Fin == 0) and (Opcode > 7) ->
            {fail_connection, "control frame may not be fragmented"};
        true ->
            ok
    end.

% no extensions are supported yet.
% http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4.2
check_reserved_bits(Unframed = #ws_frame_info{rsv=0}) ->
    check_utf8(Unframed);
check_reserved_bits(#ws_frame_info{rsv=RSV}) ->
    {fail_connection, "rsv bits were " ++ integer_to_list(RSV) ++ " but should be unset."}.

% http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id191467
% Heuristic identification of UTF-8
check_utf8(Unframed = #ws_frame_info{opcode = text, data=Bin}) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
        Bin ->
            Unframed;
        _ ->
            {fail_connection, "not valid utf-8."}
    end;
check_utf8(Unframed) ->
    check_reserved_opcode(Unframed).

check_reserved_opcode(#ws_frame_info{opcode = undefined}) ->
    {fail_connection, "Reserved opcode."};
check_reserved_opcode(Unframed) ->
    Unframed.


ws_frame_info(#ws_state{sock=Socket, endpoint=EndpointType},
              <<Fin:1, Rsv:3, Opcode:4, Masked:1, Len1:7, Rest/binary>>) ->
    case check_control_frame(Len1, Opcode, Fin) of
        ok ->
            {ws_frame_info_secondary, Length, MaskingKey, Payload, Excess}
                = ws_frame_info_secondary(Socket, Len1, Rest, EndpointType),
            FrameInfo = #ws_frame_info{fin=Fin,
                                    rsv=Rsv,
                                    opcode=opcode_to_atom(Opcode),
                                    masked=Masked,
                                    masking_key=MaskingKey,
                                    length=Length,
                                    payload=Payload},
            {FrameInfo, Excess};
        Other ->
            Other
    end;

ws_frame_info(State = #ws_state{sock=Socket}, FirstPacket) ->
    ws_frame_info(State, buffer(Socket, 2,FirstPacket)).

ws_frame_info_secondary(Socket, Len1, Rest, EndpointType) ->
    case EndpointType of
        client ->
            MaskingKeyLen = 0;
        server ->
            MaskingKeyLen = 4
    end,
    case Len1 of
        126 ->
            <<Len:16, MaskingKey:MaskingKeyLen/binary, Rest2/binary>> =
                buffer(Socket, MaskingKeyLen+2, Rest);
        127 ->
            <<Len:64, MaskingKey:MaskingKeyLen/binary, Rest2/binary>> =
                buffer(Socket, MaskingKeyLen+8, Rest);
        Len ->
            <<MaskingKey:MaskingKeyLen/binary, Rest2/binary>> =
                buffer(Socket, MaskingKeyLen, Rest)
    end,
    if
    Len > ?MAX_PAYLOAD ->
        Error = io_lib:format("Payload length ~p longer than max allowed of ~p",
                  [Len, ?MAX_PAYLOAD]),
        io:format(Error),
        exit({error, Error});
    true ->
        <<Payload:Len/binary, Excess/binary>> = buffer(Socket, Len, Rest2),
        {ws_frame_info_secondary, Len, MaskingKey, Payload, Excess}
    end.

unframe_active_once(State, FirstPacket) ->
    Frames = unframe(State, FirstPacket),
    websocket_setopts(State, [{active, once}]),
    Frames.

% Returns all the WebSocket frames fully or partially contained in FirstPacket,
% reading exactly as many more bytes from Socket as are needed to finish unframing
% the last frame partially included in FirstPacket, if needed.
%
% The length of this list and depth of this recursion is limited by
% the size of your socket receive buffer.
%
% -> { #ws_state, [#ws_frame_info,...,#ws_frame_info] }
unframe(_State, <<>>) ->
    [];
unframe(State, FirstPacket) ->
    case unframe_one(State, FirstPacket) of
        {FrameInfo = #ws_frame_info{ws_state = NewState}, RestBin} ->
            %% Every new recursion uses the #ws_state from the calling recursion.
            [FrameInfo | unframe(NewState, RestBin)];
        Fail ->
            [Fail]
    end.

% -> {#ws_frame_info, RestBin} | {fail_connection, Reason}
unframe_one(State = #ws_state{vsn=8, endpoint=EndpointType}, FirstPacket) ->
    {FrameInfo = #ws_frame_info{}, RestBin} = ws_frame_info(State, FirstPacket),
    Unmasked = case EndpointType of
                   server ->
                       mask(FrameInfo#ws_frame_info.masking_key,
                            FrameInfo#ws_frame_info.payload);
                   client ->
                       FrameInfo#ws_frame_info.payload
               end,
    NewState = frag_state_machine(State, FrameInfo),
    Unframed = FrameInfo#ws_frame_info{ data = Unmasked,
                                        ws_state = NewState },
    case checks(Unframed) of
        #ws_frame_info{} when is_record(NewState, ws_state) ->
            {Unframed, RestBin};
        #ws_frame_info{} when not is_record(NewState, ws_state) ->
            NewState;  %% pass back the error details
        Fail ->
            Fail
    end.

websocket_setopts(#ws_state{sock=Socket={sslsocket,_,_}}, Opts) ->
    ssl:setopts(Socket, Opts);
websocket_setopts(#ws_state{sock=Socket}, Opts) ->
    inet:setopts(Socket, Opts).

is_control_op(Op) ->
    atom_to_opcode(Op) > 7.

%% Unfragmented message
frag_state_machine(State = #ws_state{ frag_type = none },
                   #ws_frame_info{ fin = 1 }) ->
    State;

%% Beginning of fragmented text message
frag_state_machine(State = #ws_state{ frag_type = none },
                   #ws_frame_info{ fin = 0,
                                   opcode = text }) ->
    State#ws_state{ frag_type = text };

%% Beginning of fragmented binary message
frag_state_machine(State = #ws_state{ frag_type = none },
                   #ws_frame_info{ fin = 0,
                                   opcode = binary }) ->
    State#ws_state{ frag_type = binary };

%% Expecting text continuation
frag_state_machine(State = #ws_state{ frag_type = text },
                   #ws_frame_info{ fin = 0,
                                   opcode = continuation }) ->
    State;

%% Expecting binary continuation
frag_state_machine(State = #ws_state{ frag_type = binary },
                   #ws_frame_info{ fin = 0,
                                   opcode = continuation }) ->
    State;

%% End of fragmented text message
frag_state_machine(State = #ws_state{ frag_type = text },
                   #ws_frame_info{ fin = 1,
                                   opcode = continuation }) ->
    State#ws_state{ frag_type = none };

%% End of fragmented binary message
frag_state_machine(State = #ws_state{ frag_type = binary },
                   #ws_frame_info{ fin = 1,
                                   opcode = continuation }) ->
    State#ws_state{ frag_type = none };


frag_state_machine(State, #ws_frame_info{ opcode = Op }) ->
    IsControl = is_control_op(Op),
    if
        IsControl == true ->
            %% Control message never changes fragmentation state
            State;
        true ->
            %% Everything else is wrong
            {error, "fragmentation rules violated"}
    end.


opcode_to_atom(16#0) -> continuation;
opcode_to_atom(16#1) -> text;
opcode_to_atom(16#2) -> binary;
opcode_to_atom(16#8) -> close;
opcode_to_atom(16#9) -> ping;
opcode_to_atom(16#A) -> pong;
opcode_to_atom(_) -> undefined.

atom_to_opcode(continuation) -> 16#0;
atom_to_opcode(text) -> 16#1;
atom_to_opcode(binary) -> 16#2;
atom_to_opcode(close) -> 16#8;
atom_to_opcode(ping) -> 16#9;
atom_to_opcode(pong) -> 16#A.


frame(8, FrameType, Data, EndpointType) ->
    %FIN=true because we're not fragmenting.
    FirstByte = 128 bor atom_to_opcode(FrameType),
    ByteList = binary_to_list(Data),
    Length = length(ByteList),
    case EndpointType of
        client ->
            MaskingKey = crypto:rand_bytes(4),
            Payload = mask(MaskingKey, Data),
            MaskedBit = 1;
        server ->
            MaskingKey = <<>>,
            Payload = Data,
            MaskedBit = 0
    end,
    if
        Length < 126 ->
            << FirstByte, MaskedBit:1, Length:7, MaskingKey/binary, Payload:Length/binary >>;
        Length =< 65535 ->
            << FirstByte, MaskedBit:1, 126:7, Length:16, MaskingKey/binary, Payload:Length/binary >>;
        true ->
            Defined = Length =< math:pow(2,64),
            % TODO: Is the correctness of this pow call
            % better than the speed and danger of not checking?
            case Defined of
                true ->
                    << FirstByte, MaskedBit:1, 127:7, Length:64, MaskingKey/binary, Payload:Length/binary >>;
                _ ->
                    undefined
            end
    end.


mask(MaskBin, Data) ->
    list_to_binary(rmask(MaskBin, Data)).

%% unmask == mask. It's XOR of the four-byte masking key.
rmask(_,<<>>) ->
    [<<>>];

rmask(MaskBin = <<Mask:4/integer-unit:8>>, <<Data:4/integer-unit:8, Rest/binary>>) ->
    Masked = Mask bxor Data,
    MaskedRest = rmask(MaskBin, Rest),
    [<<Masked:4/integer-unit:8>> | MaskedRest ];

rmask(<<Mask:3/integer-unit:8, _Rest/binary>>, <<Data:3/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:3/integer-unit:8>>];

rmask(<<Mask:2/integer-unit:8, _Rest/binary>>, <<Data:2/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:2/integer-unit:8>>];

rmask(<<Mask:1/integer-unit:8, _Rest/binary>>, <<Data:1/integer-unit:8>>) ->
    Masked = Mask bxor Data,
    [<<Masked:1/integer-unit:8>>].


hash_nonce(Nonce) ->
    Salted = Nonce ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
    HashBin = crypto:sha(Salted),
    base64:encode_to_string(HashBin).
