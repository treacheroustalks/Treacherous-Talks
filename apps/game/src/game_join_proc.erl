%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
%%% @author Jan Daniel Bothma <jbothma@gmail.com>
%%%
%%% @doc Summary.
%%% @end
%%%
%%% @since :  4 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(game_join_proc).

-behaviour(gen_server).

-include_lib("utils/include/debug.hrl").

-include_lib("include/game.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/bucket.hrl").

%% API
-export([start/1, stop/1, is_alive/1, join_game/3, get_state/1, reconfig_game/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% If it called with game_players record it will directly spown the process
%% otherwise it will get game players from database based on the input game id
%% @spec start(GameId | #game_player{}) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(GamePlayer = #game_player{}) ->
    gen_server:start(?MODULE, [GamePlayer], []);
start(GameId) ->
    gen_server:start(?MODULE, [GameId], []).

stop(GameId) ->
    case get_proc(GameId, false) of
        {ok, none} ->
            ok;
        {ok, Pid} ->
            ok = game_join_proc_map:delete_pid(GameId),
            case is_alive(Pid) of
                true ->
                    gen_server:cast(Pid, {stop, self()}),
                    ok;
                false ->
                    ok
            end
    end.

join_game(GameId, UserId, Country) ->
   case get_proc(GameId, true) of
       {ok, Pid} ->
           gen_server:call(Pid, {join_game, GameId, UserId, Country});
       {error, _} = Error ->
           Error
   end.

reconfig_game(GameId, Game) ->
   case get_proc(GameId, true) of
       {ok, Pid} ->
           gen_server:call(Pid, {reconfig_game, GameId, Game});
       {error, _} = Error ->
           Error
   end.

get_state(Pid) ->
    gen_server:call(Pid, get_state).

is_alive(Pid) when is_pid(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
        true ->
            true;
        _ ->
            false
    end;
is_alive(_) ->
    false.

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
init([GamePlayersRec = #game_player{}]) ->
    case game_join_proc_map:store_pid(GamePlayersRec#game_player.id, self()) of
        ok ->
            {ok, GamePlayersRec};
        Error ->
            {stop, Error}
    end;
init([GameId]) ->
    %error_logger:info_msg("Init game joining process for game id ~p~n", [GameId]),
    case read_game_players(GameId) of
        {ok, GamePlayersRec} ->
            case game_join_proc_map:store_pid(GameId, self()) of
                ok ->
                    {ok, GamePlayersRec};
                Error ->
                    {stop, Error}
            end;
        Error ->
            {stop, Error}
    end.

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
handle_call({join_game, GameId, UserId, Country}, _From,
            GamePlayersRec=#game_player{id=GameId}) ->
    case do_join_game(GamePlayersRec, UserId, Country) of
        {ok, NewGamePlayersRec} ->
            {reply, {ok, GameId}, NewGamePlayersRec};
        {error, Reason} ->
            {reply, {error, Reason}, GamePlayersRec}
    end;

handle_call({reconfig_game, GameId, Game}, _From,
            State = #game_player{players=Players}) ->
    Reply = case Players of
                [] ->
                    update_game(GameId, Game);
                _ ->
                    {error, player_joined_this_game_already}
            end,
    {reply, Reply, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(Request, _From, GamePlayersRec) ->
    error_logger:info_msg("unhandled request ~p~n"
                          "state is ~p~n", [Request, GamePlayersRec]),
    Reply = ok,
    {reply, Reply, GamePlayersRec}.

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
handle_cast({stop, _Killer}, State) ->
    ?DEBUG("game_join_proc ~p being stopped by ~p~n", [self(), _Killer]),
    {stop, normal, State};
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
do_join_game(GamePlayersRec=#game_player{ players=Players }, UserId, Country) ->
    case user_already_joined(Players, UserId) of
        false ->
            case country_is_available(Players, Country) of
                true ->
                    NewPlayer = #game_user{id=UserId, country=Country},
                    NewPlayers = [NewPlayer|Players],
                    NewGamePlayersRec =
                        GamePlayersRec#game_player{players=NewPlayers},
                    store_game_players_new(NewGamePlayersRec, NewPlayer),
                    {ok, NewGamePlayersRec};
                false ->
                    {error, country_not_available}
            end;
        true ->
            {error, user_already_joined}
    end.


%% Players = [] | [#game_user{}]
country_is_available(GameUsers, Country) ->
    case lists:keyfind(Country, #game_user.country, GameUsers) of
        false -> % country IS available
            true;
        _ ->     % country has been joined
            false
    end.

user_already_joined(GameUsers, UserId) ->
    case lists:keyfind(UserId, #game_user.id, GameUsers) of
        false ->
            false;
        _ ->
            true
    end.

%% Store the Game Players in game record
store_game_players_new(GamePlayersRec, #game_user{id =UserId,
                                                       country=Country}) ->
    BinGameId = db:int_to_bin(GamePlayersRec#game_player.id),

    % Get the last known vclock
    {ok, DBGameObj} = db:get (?B_GAME, BinGameId, [{r,1}]),

    % Update the db object.
    % Keep vclock from the GET we just did.
    GamePropList= db_obj:get_value(DBGameObj),
    Game = data_format:plist_to_rec(?GAME_REC_NAME, GamePropList),

    Index = get_record_index(Country),
    TmpGame = setelement(Index, Game, UserId),
    TmpGamePropList = data_format:rec_to_plist(TmpGame),
    TmpGameObj = db_obj:set_value(DBGameObj, TmpGamePropList),

    % Finally write it to the DB and return
    db:put(TmpGameObj, [{w,1}]).
%    error_logger:info_msg("put result: ~p~n",[Result]).

%% reconfig game that means update the game record
update_game(Id, Game = #game{}) ->
    BinId = db:int_to_bin(Id),

    % Get the last known vclock
    {ok, DBGameObj} = db:get (?B_GAME, BinId, [{r,1}]),

    % Update the db object.
    % Keep vclock from the GET we just did.
    GamePropList = data_format:rec_to_plist(Game),
    TmpGameObj = db_obj:set_value(DBGameObj, GamePropList),

    % Finally write it to the DB and return
    GamePutResult = db:put(TmpGameObj, [{w,0}]),
    case GamePutResult of
        {error, _} = Error ->
            Error;
        _ ->
            {ok, Id}
    end.


%% Read the Game Players record, waiting until all riak nodes agree.
%% Returns {ok, GamePlayersObj} if all is cool, otherwise {error, Reason}
read_game_players(GameId) ->
    BinGameId = db:int_to_bin(GameId),
    case db:get(?B_GAME, BinGameId, [{r, all}]) of
        {error, _} = Error ->
            Error;
        {ok, GameObj} ->
            case db_obj:has_siblings(GameObj) of
                false ->
                    Game =data_format:db_obj_to_rec(GameObj, ?GAME_REC_NAME),
                    {ok, game_utils:get_game_player(Game)};
                true ->
                    % This shouldn't happen.
                    {error, "Game Players record had siblings?!?!?!"}
            end
    end.


%% Restart = true | false
get_proc(GameID, Restart) ->
    case game_join_proc_map:get_pid(GameID) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        none when Restart == true ->
            case game_worker:get_game(GameID) of
                {ok, Game = #game{status = waiting}} ->
                    {ok, GamePlayers} = game_utils:get_game_player(Game),
                    case start(GamePlayers) of
                        {ok, Pid} ->
                            {ok, Pid};
                        {error, Error} ->
                            {error, Error}
                    end;
                {ok, #game{}} ->
                    {error, game_not_in_waiting};
                {error, _} = Error ->
                    Error
            end;
        none ->
            {ok, none};
        {error, _} = Error ->
            Error
    end.

%% return the index of the country in game record
-spec get_record_index(atom()) -> integer().
get_record_index(Country) ->
    case Country of
        england ->
            #game.england;
        germany ->
            #game.germany;
        france ->
            #game.france;
        austria ->
            #game.austria;
        italy ->
            #game.italy;
        russia ->
            #game.russia;
        turkey ->
            #game.turkey
    end.
