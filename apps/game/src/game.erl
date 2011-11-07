-module (game).

-export ([new_game/1,
          get_game/1,
          delete_game/1,
          reconfig_game/1,
          join_game/3,
          get_game_players/1,
          get_game_state/2,
          phase_change/2,
          put_game_order/2,
          update_game_order/2,
          get_game_order/1
         ]).

-include_lib ("datatypes/include/game.hrl").

%% ------------------------------------------------------------------
%% Internal Macro Definitions
%% ------------------------------------------------------------------
-define(WORKER, game_worker).
-define(CAST_WORKER(Cmd), gen_server:cast(service_worker:select_pid(?WORKER), Cmd)).
-define(CALL_WORKER(Cmd), gen_server:call(service_worker:select_pid(?WORKER), Cmd)).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% @doc
%%  creates a new game asynchronously.
%%
%%  will reply {ok, GameKey :: Integer()} to the calling process
%%  in case of success
%% @end
%% -----------------------------------------------------------------------------
-spec new_game (#game{}) -> ok.
new_game(Game=#game{}) ->
    ?CALL_WORKER({new_game, Game}).

%% -----------------------------------------------------------------------------
%% @doc
%%  stores a list of game moves to the database
%%
%% @end
%% -----------------------------------------------------------------------------
put_game_order (Key, GameMove) ->
    ?CALL_WORKER({put_game_order, Key, GameMove}).

%% -----------------------------------------------------------------------------
%% @doc
%%  get a list of game moves from the database
%%
%% @end
%% -----------------------------------------------------------------------------
get_game_order (Key) ->
    ?CALL_WORKER({get_game_order, Key}).

%% -----------------------------------------------------------------------------
%% @doc
%%  stores a list of game moves to the database
%%
%% @end
%% -----------------------------------------------------------------------------
update_game_order (Key, GameMove) ->
    ?CALL_WORKER({update_game_order, Key, GameMove}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game from the database and returns it asynchronously
%%
%%  will reply {ok, #game{}} to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
get_game(Key) ->
    ?CALL_WORKER({get_game, Key}).

%% -----------------------------------------------------------------------------
%% @doc
%%  deletes a game from the database asynchronously
%%
%%  will reply ok to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
delete_game(Key) ->
    ?CALL_WORKER({delete_game, Key}).
%% -----------------------------------------------------------------------------
%% @doc
%%  reconfig a game from the database asynchronously
%%
%%  will reply {ok, GameKey :: Integer()} to the calling process
%% in case of success
%% @end
%% -----------------------------------------------------------------------------
reconfig_game(Game = #game{}) ->
    ?CALL_WORKER({reconfig_game, Game}).

% -----------------------------------------------------------------------------
%% @doc
%%  join a player to a game from the database asynchronously
%%
%%  will reply {ok, GameKey :: Integer()} to the calling process
%% in case of success
%%  if country is already taken reply {error, country_not_available}
%% @end
%% -----------------------------------------------------------------------------
join_game(GameID, UserID, Country) ->
    ?CALL_WORKER({join_game, GameID, UserID, Country}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game players from the database and returns it asynchronously
%%
%%  will reply {ok, #game{}} to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
get_game_players(GameID) ->
    ?CALL_WORKER({get_game_player, GameID}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game status from the database and returns it asynchronously
%%
%%  will reply {ok, #game_status{}} to the calling process in case of success
%% if the user is not joined the game reply {error, user_not_play_this_game}
%% this function only return state of waiting game and if the game is not in
%% waiting, it will reply {error, game_not_waiting}
%% @end
%% -----------------------------------------------------------------------------
get_game_state(GameID, UserID) ->
    ?CALL_WORKER({get_game_state, GameID, UserID}).
%% -----------------------------------------------------------------------------
%% @doc
%%  API for handling phase changes
%% @end
%% ----------------------------------------------------------------------------
phase_change(Game, NewPhase) ->
    ?CAST_WORKER({phase_change, Game, NewPhase}).
