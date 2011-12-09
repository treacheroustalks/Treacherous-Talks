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
-module(game_worker).
-behaviour(gen_server).

-include_lib("utils/include/debug.hrl").

-include_lib ("datatypes/include/game.hrl").
-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("datatypes/include/message.hrl").
-include_lib ("datatypes/include/push_event.hrl").
-include_lib ("datatypes/include/user.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Exports for eunit and game timer
-export([get_game_order_key/2, get_target_players/3, send_game_msg/3]).

%% ------------------------------------------------------------------
%% External exports
%% ------------------------------------------------------------------
-export([get_game/1, update_game/2]).

%% server state
-record(state, {}).

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

-spec init (no_arg) -> no_return ().
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};

handle_call({put_game_order, GameId, UserId, GameOrderList}, _From, State) ->
    Reply = put_game_order(GameId, UserId, GameOrderList),
    {reply, Reply, State};

handle_call({game_msg, Message = #game_message{}, ToCountries, Role}, _From, State) ->
    Reply = send_game_msg(Message, ToCountries, Role),
    {reply, Reply, State};

handle_call({new_game, Game=#game{id = ID}}, _From, State) ->
    Result = new_game(ID, Game),
    {ok, NewGame} = Result,
    NewID = NewGame#game.id,
    game_timer_sup:create_timer(NewGame),
    game_timer:event(NewID, start),
    {reply, {ok, NewID}, State};
handle_call({restart_game, Game}, _From, State) ->
    game_timer_sup:create_timer(Game),
    game_timer:event(Game#game.id, restart),
    {reply, {ok, Game#game.id}, State};
handle_call({stop_game, ID}, _From, State) ->
    Reply = try game_timer:stop(ID, stopped)
            catch
                % If no process found, restart the timer and then stop the game.
                % We should not reach here ideally since all games must have
                % timers.
                exit:_Error ->
                    {ok, Game} = get_game(ID),
                    game_timer_sup:create_timer(Game),
                    game_timer:event(ID, restart),
                    game_timer:stop(ID, stopped)
            end,
    {reply, Reply, State};
handle_call({reconfig_game, Game=#game{id = ID}}, _From, State) ->
    Reply = update_game(ID, Game),
    game_timer:event(ID, {reconfig, Game}),
    {reply, Reply, State};
handle_call({get_game, ID}, _From, State) ->
    Reply = get_game(ID),
    {reply, Reply, State};
handle_call({get_keys, Field, Value}, _From, State) ->
    Reply = game_utils:get_keys(Field, Value),
    {reply, Reply, State};

handle_call({join_game, GameID, UserID, Country}, _From, State) ->
    Reply = game_join_proc:join_game(GameID, UserID, Country),
    {reply, Reply, State};
handle_call({get_game_player, GameID}, _From, State) ->
    Reply = get_game_player(GameID),
    {reply, Reply, State};
handle_call({get_game_overview, GameID, UserID}, _From, State) ->
    Reply = get_game_overview(GameID, UserID),
    {reply, Reply, State};
handle_call({delete_game, Key}, _From, State) ->
    BinKey = db:int_to_bin(Key),
    Reply = db:delete(?B_GAME, BinKey),
    {reply, Reply, State};
handle_call({get_current_game, ID}, _From, State) ->
    Reply = game_utils:get_current_game(ID),
    {reply, Reply, State};
handle_call({search, Query},_From, State) ->
    Reply = db_utils:do_search(?B_GAME, Query),
    {reply, Reply, State};
handle_call({get_games_current, UserID},_From, State) ->
    Reply = get_games_current(UserID),
    {reply, Reply, State};
handle_call({get_game_search, Query},_From, State) ->
    Reply = get_game_search(Query),
    {reply, Reply, State};
handle_call(get_games_ongoing,_From, State) ->
    Reply = get_games_ongoing(),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    ?DEBUG("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    ?DEBUG("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Creates a new game record in the database.
%% The first arguments being the atom 'undefined' will create a new
%% ID for the game.
%% @spec
%% new_game(ID :: atom() | integer(), Game :: #game{}) ->
%%     {ok, #game{}} | Error
%% @end
%%-------------------------------------------------------------------
new_game(undefined, #game{} = Game) ->
    ID = db:get_unique_id(),
    new_game(ID, Game#game{id = ID});
new_game(ID, #game{} = Game) ->
    BinID = db:int_to_bin(ID),
    % Store Game record as proplist for search
    Game2 = Game#game{date_created = erlang:universaltime()},
    GamePropList = data_format:rec_to_plist(Game2),
    DBGameObj=db_obj:create(?B_GAME, BinID, GamePropList),
    GamePlayersRec = #game_player{id=ID},
    DBGamePlayerObj=db_obj:create (?B_GAME_PLAYER, BinID, GamePlayersRec),
    GamePutResult = db:put (DBGameObj, [{w,1}]),
    case GamePutResult of
        {error, _} = Error ->
            Error;
        _ ->
            PlayersPutResult = db:put (DBGamePlayerObj, [{w, 1}]),
            case PlayersPutResult of
                {error, _} = Error ->
                    Error;
                _ ->
                    {ok, _Pid} = game_join_proc:start(GamePlayersRec),
                    {ok, Game2}
            end
    end.

%%-------------------------------------------------------------------
%% @doc
%% Updates a game with the given ID with the given game record in the database.
%% @spec
%% update_game(ID :: integer(), Game :: #game{}) ->
%%     {ok, ID} | Error
%% @end
%%-------------------------------------------------------------------
update_game(ID, #game{} = Game) ->
    BinID = db:int_to_bin(ID),
    % Store Game record as proplist for search
    GamePropList = data_format:rec_to_plist(Game),
    DBGameObj=db_obj:create(?B_GAME, BinID, GamePropList),
    GamePutResult = db:put (DBGameObj, [{w,1}]),
    case GamePutResult of
        {error, _} = Error ->
            Error;
        _ ->
            {ok, ID}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Creates a game overview, based on the data of the user and given
%% game ID. Only a user who is in a game can see an overview of it when
%% it is ongoing, but anyone can get an overview of a finished game.
%% @spec
%% get_game_overview(GameID :: integer(), UserID :: integer()) ->
%%     GameOverview :: #game_overview{}
%%     | {error, game_not_started}
%%     | {error, user_not_playing_this_game}
%%     | {error, game_stopped}
%%     | Error
%% @end
%%-------------------------------------------------------------------
get_game_overview(GameID, UserID) ->
    case get_playercountry_game(GameID, UserID) of
        {ok, Country, #game{status = ongoing} = Game} ->
            Key = get_game_order_key(GameID, Country),
            Orders = game_utils:get_game_order(Key),
            OV = basic_game_overview(GameID, Game),
            {ok, OV#game_overview{order_list = Orders,
                                  country = Country}
            };
        {_, _Player, #game{status = finished} = Game} ->
            % user that is not in the game can view a finished game
            OV = basic_game_overview(GameID, Game),
            {ok, OV#game_overview{players =
                                  game_utils:userlist(GameID)}
            };
        {ok, _Country, #game{status = waiting}} ->
            % game not started yet, or stopped.
            {error, game_not_started};
        {ok, _Country, #game{status = stopped}} ->
            {error, game_stopped};
        {no_player, PlayerError, _Game} ->
            PlayerError;
        {no_game, _Player, GameError} ->
            GameError
    end.
%%-------------------------------------------------------------------
%% @doc
%% Creates basic game overview, only based on game and gamestate
%% for a finished game, this will present the last state of the game
%% @spec
%% basic_game_overview(GameID :: integer(), Game :: #game{}) ->
%%            #game_overview{}
%% @end
%%-------------------------------------------------------------------
basic_game_overview(GameID, Game) ->
    {ok, GameState} = game_utils:get_game_state(GameID),
    #game_overview{game_rec = Game,
                   map = GameState#game_state.map,
                   phase = GameState#game_state.phase,
                   year_season = GameState#game_state.year_season}.

%% ------------------------------------------------------------------
%% @doc
%% Stores a game order
%% GameId::integer(), UserId::integer()
%% @spec
%%        put_game_order(GameID :: integer(),
%%                       UserId :: integer(),
%%                       GameOrderList :: list(tuple())) ->
%%               {ok, Key :: string()} | {error, game_id_not_exist} | Error
%% @end
%% ------------------------------------------------------------------
put_game_order(GameId, UserId, GameOrderList) ->
    case get_playercountry_game(GameId, UserId) of
        {ok, Country, _Game} ->
            Key = get_game_order_key(GameId, Country),
            NewOrder = game_utils:translate_game_order(GameId, GameOrderList,
                                                    Country),
            % if there are no previous orders, use put, otherwise update
            case game_utils:get_game_order (Key) of
                [] ->
                    put_game_order(Key, NewOrder);
                _GO ->
                    update_game_order(Key, NewOrder)
            end;
        {no_player, PlayerError, _Game} ->
            PlayerError;
        {no_game, _Player, GameError} ->
            GameError
    end.
%% ------------------------------------------------------------------
%% @doc
%% Stores a game order
%% GameId::integer(), UserId::integer()
%% @spec
%%        put_game_order(Key :: string(), GameOrderList :: list(tuple())) ->
%%               {ok, Key :: atom()}
%% @end
%% ------------------------------------------------------------------
put_game_order(Key, GameOrderList) ->
    BinID = list_to_binary(Key),
    DBGameOrderObj = db_obj:create (?B_GAME_ORDER,
                                    BinID,
                                    #game_order{order_list=GameOrderList}),
    spawn (fun() -> db:put (DBGameOrderObj) end),
    {ok, Key}.

%% ------------------------------------------------------------------
%% @doc
%% Updates a game order in the database
%% @spec
%% update_game_order(ID :: string(), NewOrder :: list(tuple())) ->
%%     {ok, Key :: atom()} | {error, Error}
%% @end
%% ------------------------------------------------------------------
update_game_order(ID, NewOrder) ->
    case db:get(?B_GAME_ORDER, list_to_binary(ID), [{r,1}]) of
        {ok, Obj} ->
            game_utils:update_db_obj(Obj,
                                     #game_order{order_list = NewOrder},
                                     [{w, 1}]),
            {ok, ID};
        Error ->
            {error, Error}
    end.

%% ------------------------------------------------------------------
%% @doc
%% Checks if a player is participating in a specific game, and which country
%% @spec
%% get_playercountry_game(GameId :: integer(), UserId :: integer()) ->
%%     {ok, Country, Game}
%%     | {no_player, {error, user_not_playing_this_game}, Game}
%%     | {no_game, UserId, {error, game_id_not_exist}}
%% @end
%% ------------------------------------------------------------------
get_playercountry_game(GameId, UserId) ->
    case get_game(GameId) of
        {ok, Game} ->
            case get_player_country(GameId, UserId) of
                {ok, Country} ->
                    {ok, Country, Game};
                Error ->
                    {no_player, Error, Game}
            end;
        _Error ->
            {no_game, UserId, {error, game_id_not_exist}}
    end.
%% ------------------------------------------------------------------
%% @doc Returns the country atom which a user is playing in a game
%% @spec
%%        get_player_country(GameID :: integer(), UserID :: integer()) ->
%%               {ok, Country :: atom()} | {error, user_not_playing_this_game}
%% @end
%% ------------------------------------------------------------------
get_player_country (GameID, UserID) ->
    case get_game_player(GameID) of
        {ok, GPRec = #game_player{}} ->
            case lists:keyfind(UserID, #game_user.id,
                               GPRec#game_player.players) of
                false ->
                    {error, user_not_playing_this_game};
                GU = #game_user{} ->
                    {ok, GU#game_user.country}
            end;
        Other ->
            Other

    end.

%% ------------------------------------------------------------------
%% @doc Returns a list of game player by specifying their countries and ID
%%
%% @end
%% ------------------------------------------------------------------
-spec get_target_players({integer(), integer()}, [atom()], role()) ->
          {ok, {atom(), [#game_user{}]}} | {error, any()}.
get_target_players({FromID, GameID}, CountryList, Role) ->
    case get_game_player(GameID) of
        {ok, GPRec = #game_player{}} ->
            Players = GPRec#game_player.players,
            case game_utils:is_power_user(Role) of
                false ->
                    case get_country_by_id(FromID, Players) of
                        GU = #game_user{} ->
                            FromCountry = GU#game_user.country,
                            ToGUList = get_target_players(CountryList, Players, []),
                            {ok, {FromCountry, ToGUList}};
                        Error ->
                            Error
                    end;
                _PowerUser ->
                    ToGUList = get_target_players(CountryList, Players, []),
                    {ok, {Role, ToGUList}}
            end;
        Other ->
            Other
    end;
get_target_players(_, [], AccGUList) ->
    AccGUList;
get_target_players(CountryList, [GU = #game_user{country=C}|Rest],AccGUList)->
    case lists:member(C, CountryList) of
        true ->
            get_target_players(CountryList, Rest, [GU|AccGUList]);
        false ->
            get_target_players(CountryList, Rest, AccGUList)
    end.

get_country_by_id(UserID, Players) ->
    case lists:keyfind(UserID, #game_user.id, Players) of
        false ->
            {error, user_not_playing_this_game};
        GameUser = #game_user{} ->
            GameUser
    end.


%% ------------------------------------------------------------------
%% @doc
%% Builds the key for getting orders of a country.
%% example: get_game_order_key(12345, england) ->
%%                 "12345-1901-fall-order_phase-england".
%% @spec
%% get_game_order_key(ID :: integer(), Country :: atom()) ->
%%     Key
%% @end
%% ------------------------------------------------------------------
get_game_order_key(Id, Country) ->
    game_utils:get_keyprefix({id, Id}) ++ "-" ++ atom_to_list(Country).

%% ------------------------------------------------------------------
%% @doc Returns the game with id ID
%% @spec
%% get_game(GameID :: integer()) ->
%%     {ok, Game :: #game{}} | Error
%% @end
%% ------------------------------------------------------------------
get_game(ID)->
    case game_utils:get_db_obj(?B_GAME, db:int_to_bin(ID), [{r,1}]) of
        {ok, GamePropList} ->
            Game = data_format:plist_to_rec(?GAME_REC_NAME, GamePropList),
            {ok, Game};
        {error, Error} ->
            {error, Error}
    end.

%% ------------------------------------------------------------------
%% @doc Returns the game_player record of a game
%% @spec
%%        get_game_player(GameID :: integer()) ->
%%               {ok, GamePlayer :: #game_player{}} | Error
%% @end
%% ------------------------------------------------------------------
get_game_player(GameID)->
    game_utils:get_db_obj(?B_GAME_PLAYER, GameID, [{r,1}]).


%%-------------------------------------------------------------------
%% @doc
%% Get the games for the given user with status = waiting | ongoing
%% @end
%%-------------------------------------------------------------------
-spec get_games_current(integer()) -> {ok, [#game{}]}.
get_games_current(UserID) ->
    Query = "id=" ++ integer_to_list(UserID) ++ " AND "
            "(status=waiting OR status=ongoing)",
    db_utils:do_search_values(?B_GAME, Query, ?GAME_REC_NAME).

%%-------------------------------------------------------------------
%% @doc
%% Get the games for the given search query
%% @end
%%-------------------------------------------------------------------
-spec get_game_search(string()) -> {ok, [#game{}]}.
get_game_search(Query) ->
    db_utils:do_search_values(?B_GAME, Query, ?GAME_REC_NAME).

%%-------------------------------------------------------------------
%% @doc
%% Get all the ongoing games
%% @end
%%-------------------------------------------------------------------
-spec get_games_ongoing() -> {ok, [integer()]}.
get_games_ongoing() ->
    Query = "status=ongoing",
    db_utils:do_search(?B_GAME, Query).

%% ------------------------------------------------------------------
%% @doc
%% Send an in-game message to other players, which players to send
%% depends on which countries have been specified at the frontend
%%
%% @end
%% ------------------------------------------------------------------
-spec send_game_msg(#game_message{}, list(), role()) ->{ok, integer()} |
                                               {error, game_phase_not_ongoing} |
                                               {error, user_not_playing_this_game}|
                                               {error, not_allowed_send_msg} |
                                               {error, any()}.
send_game_msg(GMsg = #game_message{game_id = GameId,
                                   from_id = FromId}, ToCountries, Role) ->
    case get_game(GMsg#game_message.game_id) of
        {ok, #game{press = Press, status = ongoing}} ->
            case get_target_players({FromId, GameId}, ToCountries, Role) of
                {ok, {From, ToGameUserList}} ->
                    Date = erlang:universaltime(),
                    % from_country can be a moderator or operator too
                    NewGMsg = GMsg#game_message{
                                                from_country = From,
                                                date_created = Date},
                    send_msg_to_msgapp(ToGameUserList, NewGMsg, Press, Role);
                Other ->
                    Other
            end;
        {ok, #game{}} ->
            {error, game_phase_not_ongoing};
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   send a message to a list of game_user, which is supposed to receive
%%    the message. The message is sent via the message application.
%% @end
%%------------------------------------------------------------------------------
-spec send_msg_to_msgapp([#game_user{}], #game_message{}, atom(), role()) ->
          ok | {error, not_allowed_send_msg}.
send_msg_to_msgapp([], #game_message{game_id = GameID}, _, _Role) ->
    {ok, GameID};
send_msg_to_msgapp([#game_user{id= ID, country = Country}|Rest],
                   GMsg = #game_message{}, Press, Role) ->
    case game_utils:is_power_user(Role) of
        false ->
            case Press of
                white ->
                    NewGMsg = GMsg#game_message{to_id = ID, to_country = Country},
                    message:game_msg(NewGMsg),
                    send_msg_to_msgapp(Rest, GMsg, Press, Role);
                grey ->
                    NewGMsg = GMsg#game_message{to_id = ID, to_country = Country,
                                                from_country = unknown},
                    message:game_msg(NewGMsg),
                    send_msg_to_msgapp(Rest, GMsg, Press, Role);
                none ->
                    {error, not_allowed_send_msg}
            end;
        _PowerUser ->
            % we don't care about press
            NewGMsg = GMsg#game_message{to_id = ID, to_country = Country},
            message:game_msg(NewGMsg),
            send_msg_to_msgapp(Rest, GMsg, Press, Role)
    end.
