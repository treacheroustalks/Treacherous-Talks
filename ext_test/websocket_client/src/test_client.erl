%%%-------------------------------------------------------------------
%%%
%%% @copyright (C)
%%% @doc
%%%  This module is both a gen_server callback module,
%%%  and a websocket_common callback. This is bad practise and should be
%%%  refactored ASAP.
%%%
%%%  The purpose of this module is to have a synchronous interface for
%%%  a websocket client for testing out websocket interface.
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(test_client).

-behaviour(gen_server).

%% API
-export([connect/2, recv/0, send/1, close/0]).

%% Export for websocket_client
-export([handle_message/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { messages = [],
                 receiving = undefined,
                 ws_pid = undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================


connect(Host, Port) ->
    Path = "/endpoint",
    Origin = "",
    {ok, WSPid} = websocket_client:connect(Host, Port, Path, Origin, ?MODULE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WSPid], []).

%% -> {Type, Data}
recv() ->
    gen_server:call(?MODULE, recv).

%% send({Type, Data})
send(Message) ->
    gen_server:cast(?MODULE, {send, Message}).

close() ->
    gen_server:cast(?MODULE, stop).


%%%===================================================================
%%% websocket callbacks
%%%===================================================================

%%------------------------------------------------------------------------------
%% Type :: text|binary
%% Data :: binary()
%% HandlerResult :: {reply, {Type, Data}}
%%                | {noreply}
%%                | {close, Reason}
%% handle_message({Type, Data}) -> HandlerResult
%%------------------------------------------------------------------------------
handle_message(Message) ->
%    io:format(standard_error, "server said ~p~n", [Data]),
    gen_server:cast(?MODULE, {deliver, Message}),
    {noreply}.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([WSPid]) ->
    {ok, #state{ messages = [],
                 receiving = undefined,
                 ws_pid = WSPid
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(recv, From, State = #state{messages=[]}) ->
    NewState = State#state{ receiving = From },
    {noreply, NewState};
handle_call(recv, _From, State = #state{messages=[Message|Rest]}) ->
    NewState = State#state{ messages = Rest },
    Reply = Message,
    {reply, Reply, NewState};
handle_call(Request, From, State) ->
    io:format("unhandled call ~p from ~p~n", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({deliver, Message}, State = #state{ receiving=undefined,
                                             messages=Messages}) ->
    NewMessageList = lists:append(Messages, [Message]),
    NewState = State#state{messages = NewMessageList},
    {noreply, NewState};
handle_cast({deliver, Message}, State = #state{ receiving=From,
                                                messages=[]}) ->
    gen_server:reply(From, Message),
    NewState = State#state{receiving = undefined},
    {noreply, NewState};
handle_cast({send, Message}, State = #state{ws_pid = WSPid}) ->
    websocket_common:send(WSPid, Message),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    io:format("unhandled cast ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unhandled message ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%connect(wsorg) ->
%    Host = "echo.websocket.org",
%    Port = 80,
%    Path = "/",
%    Origin = "http://websocket.org",
%    websocket_client:connect(Host, Port, Path, Origin, ?MODULE);
%connect(jbothma) ->
%    Host = "jbothma.co.uk",
%    Port = 8000,
%    Path = "/websockets_example_endpoint.yaws",
%    Origin = "http://jbothma.co.uk",
%    websocket_client:connect(Host, Port, Path, Origin, ?MODULE);
%connect(localtt) ->
%    Host = "127.0.0.1",
%    Port = 8000,
%    Path = "/endpoint",
%    Origin = "",
%    websocket_client:connect(Host, Port, Path, Origin, ?MODULE);

