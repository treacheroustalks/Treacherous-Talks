-module (game).

-export ([new_game/1,
          get_game/1,
          get_keys_by_idx/2,
          delete_game/1,
          reconfig_game/1,
          join_game/3,
          get_game_players/1,
          get_game_state/2,
          phase_change/2,
          get_current_game/1,
          process_phase/2,
          put_game_order/3
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
put_game_order (GameId, UserId, GameOrder) ->
    ?CALL_WORKER({put_game_order, GameId, UserId, GameOrder}).

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
%% Gets Field and Value as arguments, returns {ok, [Key1, Key2, ..., KeyN]}
%% or {error, Error}.
%% Field is #record.field,
%% Value is the value to search for.
%% @end
%% -----------------------------------------------------------------------------
get_keys_by_idx(Field, Value) ->
    ?CALL_WORKER({get_keys_by_idx, Field, Value}).

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

%% -----------------------------------------------------------------------------
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
%%  gets the current game information of a game from the database
%%  will reply {ok, game_current{}} to the calling process in case of success
%% @end
%% ----------------------------------------------------------------------------
get_current_game(ID) ->
    ?CALL_WORKER({get_current_game, ID}).

%% -----------------------------------------------------------------------------
%% @doc
%%  Handles the game phase and updates the state of the game, replies
%%  {ok, Phase} if the game is to continue or {ok, end} if the game has ended
%% @end
%% ----------------------------------------------------------------------------
process_phase(ID, Phase) ->
    ?CALL_WORKER({process_phase, ID, Phase}).
%% -----------------------------------------------------------------------------
%% @doc
%%  API for handling phase changes
%% @end
%% ----------------------------------------------------------------------------
phase_change(Game, NewPhase) ->
    ?CALL_WORKER({phase_change, Game, NewPhase}).
