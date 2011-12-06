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
-module (game).

-export ([new_game/1,
          restart_game/1,
          get_game/1,
          get_keys_by_idx/2,
          delete_game/1,
          reconfig_game/1,
          join_game/3,
          get_game_players/1,
          get_game_overview/2,
          put_game_order/3,
          get_current_game/1,
          search/1, get_game_search/1,
          get_games_current/1,
          game_msg/3,
          stop_game/1
         ]).

-include_lib ("datatypes/include/game.hrl").
-include_lib ("datatypes/include/message.hrl").
-include_lib ("datatypes/include/user.hrl").

%% ------------------------------------------------------------------
%% Internal Macro Definitions
%% ------------------------------------------------------------------
-define(WORKER, game_worker).
-define(CAST_WORKER(Cmd), gen_server:cast(service_worker:select_pid(?WORKER), Cmd)).
-define(CALL_WORKER(Cmd), try gen_server:call(
                                service_worker:select_pid(?WORKER), Cmd)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% @doc
%%  creates a new game
%% @spec
%% new_game(Game :: #game{}) ->
%%     {ok, ID :: integer} | Error
%% @end
%% -----------------------------------------------------------------------------
new_game(Game=#game{}) ->
    ?CALL_WORKER({new_game, Game}).

%% -----------------------------------------------------------------------------
%% @doc
%%  restarts a game timer
%% @spec
%% restart_game(Game :: #game{}) ->
%%     {ok, ID :: integer} | Error
%% @end
%% -----------------------------------------------------------------------------
restart_game(Game=#game{}) ->
    ?CALL_WORKER({restart_game, Game}).

%% -----------------------------------------------------------------------------
%% @doc
%%  stores a list of game moves to the database
%% @spec
%% put_game_order(GameId :: integer(),
%%                UserId :: integer(),
%%                GameOrder :: list()) ->
%%     {ok, ID :: string()} | Error
%% @end
%% -----------------------------------------------------------------------------
put_game_order (GameId, UserId, GameOrder) ->
    ?CALL_WORKER({put_game_order, GameId, UserId, GameOrder}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game from the database and returns it
%% @spec
%% get_game(Key :: integer()) ->
%%     {ok, Game :: #game{}} | Error
%% @end
%% -----------------------------------------------------------------------------
get_game(Key) ->
    ?CALL_WORKER({get_game, Key}).

%% -----------------------------------------------------------------------------
%% @doc
%% Gets Field and Value as arguments, returns {ok, [Key1, Key2, ..., KeyN]}
%% or {error, Error}.
%% Field is #record.field,
%% Value is the value to search for.
%% @spec
%% get_keys_by_idx(Field :: any(), Value :: any()) ->
%%     {ok, Keys :: list()} | {error, Error}
%% @end
%% -----------------------------------------------------------------------------
get_keys_by_idx(Field, Value) ->
    ?CALL_WORKER({get_keys_by_idx, Field, Value}).

%% -----------------------------------------------------------------------------
%% @doc
%%  deletes a game from the database
%% @spec
%% delete_game(Key :: integer()) -> ok
%% @end
%% -----------------------------------------------------------------------------
delete_game(Key) ->
    ?CALL_WORKER({delete_game, Key}).

%% -----------------------------------------------------------------------------
%% @doc
%%  reconfig a game from the database
%%
%%  will reply {ok, GameKey :: Integer()} to the calling process
%% in case of success
%% @spec
%% reconfig_game(Game :: #game{}) -> {ok, ID :: integer} | Error
%% @end
%% -----------------------------------------------------------------------------
reconfig_game(Game = #game{}) ->
    ?CALL_WORKER({reconfig_game, Game}).

%% -----------------------------------------------------------------------------
%% @doc
%%  join a player to a game from the database
%%
%%  will reply {ok, GameKey :: Integer()} to the calling process
%% in case of success
%%  if country is already taken reply {error, country_not_available}
%% @spec
%% join_game(GameID :: integer(), UserID :: integer, Country :: country()) ->
%%     {ok, GameID :: integer()} | {error, country_not_available}
%% @end
%% -----------------------------------------------------------------------------
join_game(GameID, UserID, Country) ->
    ?CALL_WORKER({join_game, GameID, UserID, Country}).


%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game players from the database and returns it
%% @spec
%% get_game_players(GameID :: integer) ->
%%     {ok, GamePlayers :: #game_player{}} | Error
%% @end
%% -----------------------------------------------------------------------------
get_game_players(GameID) ->
    ?CALL_WORKER({get_game_player, GameID}).

%% -----------------------------------------------------------------------------
%% @doc
%% gets a game overview for a given game and a specific user
%% @spec
%% get_game_overview(GameID :: integer(), UserID :: integer()) ->
%%     {ok, GameOverview :: #game_overview{}} | {error, game_not_started}
%% @end
%% -----------------------------------------------------------------------------
get_game_overview(GameID, UserID) ->
    ?CALL_WORKER({get_game_overview, GameID, UserID}).

%% -----------------------------------------------------------------------------
%% @doc
%%  get the game_current record for the game with id GameID
%% replies {ok, CurrentGame :: #game_current{}} or Error.
%% @spec
%% get_current_game(GameID :: integer()) ->
%%     {ok, CurrentGame :: #game_current{}} | Error
%% @end
%% -----------------------------------------------------------------------------
get_current_game(GameID) ->
    ?CALL_WORKER({get_current_game, GameID}).

%% -----------------------------------------------------------------------------
%% @doc
%%  Search game bucket.
%%  See http://wiki.basho.com/Riak-Search---Querying.html for query syntax
%%  ORed together and NotParams are passed as NOT
%% @spec
%% search(Query :: string()) ->
%%     {ok, [integer()]} | {error, Error}
%% @end
%% ----------------------------------------------------------------------------
search(Query) ->
    ?CALL_WORKER({search, Query}).

%% -----------------------------------------------------------------------------
%% @doc
%%  Get the games for the given user with status = waiting | ongoing
%% @spec
%% get_games_current(UserID :: integer()) ->
%%     {ok, [#game{}]} | {error, Error}
%% @end
%% ----------------------------------------------------------------------------
get_games_current(UserID) ->
    ?CALL_WORKER({get_games_current, UserID}).

%% -----------------------------------------------------------------------------
%% @doc
%%  Get the games for the given search query
%% search(Query :: string()) ->
%%     {ok, [#game{}]} | {error, Error}
%% @end
%% ----------------------------------------------------------------------------
get_game_search(Query) ->
    ?CALL_WORKER({get_game_search, Query}).

%% -----------------------------------------------------------------------------
%% @doc
%%   Send in-game message to relevant players
%% it could reply: ok |
%%                 {error, game_phase_not_ongoing} |
%%                 {error, user_not_playing_this_game}|
%%                 {error, not_allowed_send_msg} |
%%                 {error, any()}
%%
%% @end
%% ----------------------------------------------------------------------------
-spec game_msg(#game_message{}, list(), role()) ->ok |
                                          {error, game_phase_not_ongoing} |
                                          {error, user_not_playing_this_game}|
                                          {error, not_allowed_send_msg} |
                                          {error, any()}.
game_msg(Message= #game_message{}, ToCountries, Role) ->
    ?CALL_WORKER({game_msg, Message, ToCountries, Role}).

%% -----------------------------------------------------------------------------
%% @doc
%%  changes the status of a game to stopped and kills its game timer
%% @spec
%% stop_game(GameID :: integer()) -> {ok, {GameID, stopped}}
%% @end
%% -----------------------------------------------------------------------------
stop_game(GameID) ->
    ?CALL_WORKER({stop_game, GameID}).
