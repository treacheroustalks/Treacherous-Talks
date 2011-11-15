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

-include_lib("include/game.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/bucket.hrl").

%% API
-export([start/1, stop/1, is_alive/1, join_game/3, get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(GameId) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
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
       {error, Error} ->
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
init([GameId]) ->
%    error_logger:info_msg("Init game joining process for game id ~p~n", [GameId]),
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
handle_cast({stop, Killer}, State) ->
    io:format("game_join_proc ~p being stopped by ~p~n", [self(), Killer]),
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
                    store_game_players_link_new(NewGamePlayersRec, UserId),
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

%% Store the Game Players record with a link added to the new
%% newly added user.
store_game_players_link_new(GamePlayersRec, UserId) ->
    BinGameId = db:int_to_bin(GamePlayersRec#game_player.id),

    % Get the last known vclock
    {ok, DBGamePlayersObj} = db:get (?B_GAME_PLAYER, BinGameId),

    % Update the db object.
    % Keep vclock from the GET we just did.
    TmpGamePlayersObj = db_obj:set_value(DBGamePlayersObj,
                                         GamePlayersRec),
    % Add a link to the user
    GamePlayersObjLinked = db_obj:add_link(TmpGamePlayersObj,
                                   {{?B_USER, db:int_to_bin(UserId)},
                                    ?GAME_PLAYER_LINK_USER}),

    % Finally write it to the DB and return
    db:put(GamePlayersObjLinked).
%    error_logger:info_msg("put result: ~p~n",[Result]).



%% Read the Game Players record, waiting until all riak nodes agree.
%% Returns {ok, GamePlayersObj} if all is cool, otherwise {error, Reason}
read_game_players(GameId) ->
    BinGameId = db:int_to_bin(GameId),
    case db:get(?B_GAME_PLAYER, BinGameId, [{r, all}]) of
        {error, _} = Error ->
            Error;
        {ok, GamePlayersObj} ->
            case db_obj:has_siblings(GamePlayersObj) of
                false ->
                    {ok, db_obj:get_value(GamePlayersObj)};
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
            case start(GameID) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        none ->
            {ok, none};
        {error, _} = Error ->
            Error
    end.
