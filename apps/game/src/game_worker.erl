-module(game_worker).
-behaviour(gen_server).

-include_lib ("datatypes/include/game.hrl").
-include_lib ("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% server state
-record(state, {client}).

get_client (#state{client=Client}) ->
    Client.
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

ping() ->
    gen_server:call(service_worker:select_pid(?MODULE), ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, Riak} = (application:get_env (game, riak)),
    {ok, Client} = db_c:connect (Riak),
    {ok, #state{client=Client}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call(_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast({new_game, From, Game=#game{id=ID}}, State) ->
    DBObj=db_obj:create (?GAME_BUCKET, ID, Game),
    DBReply = db_c:put (get_client (State), DBObj),
    case DBReply of
        ok ->
            gen_server:reply (From, {ok, ID}),
            {noreply, State};
        {ok, AssignedID} ->
            gen_server:reply (From, {ok, AssignedID}),
            {noreply, State}
    end;
handle_cast ({get_game, From, Key}, State) ->
    DBReply = db_c:get (get_client (State), ?GAME_BUCKET, Key),
    case DBReply of 
        {ok, DBObj} -> 
            gen_server:reply (From, 
                              {ok, db_obj:get_value (DBObj)});
        Other ->
            gen_server:reply (From, Other)
    end,
    {noreply, State};
handle_cast ({delete_game, From, Key}, State) ->
    gen_server:reply (From, db_c:delete (get_client (State), ?GAME_BUCKET, Key)),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format ("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

