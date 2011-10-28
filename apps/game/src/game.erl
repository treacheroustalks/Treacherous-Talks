-module (game).

-type tag () :: {pid (), any ()}.

-export ([new_game/2,
          get_game/2,
          delete_game/2,
          update_game/2,
          join_game/4,
          get_game_players/2
         ]).

-include_lib ("datatypes/include/game.hrl").

%% -----------------------------------------------------------------------------
%% @doc
%%  creates a new game asynchronously.
%%
%%  will reply {tag (), {ok, GameKey :: Integer()}} to the calling process
%%  in case of success
%% @end
%% -----------------------------------------------------------------------------
-spec new_game (tag (), #game{}) -> ok.
new_game (From, Game=#game{}) ->
    gen_server:cast (service_worker:select_pid (game_worker),
                     {new_game, From, Game}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game from the database and returns it asynchronously
%%
%%  will reply {tag (), {ok, #game{}}} to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
get_game (From, Key) ->
    gen_server:cast (service_worker:select_pid (game_worker),
                     {get_game, From, Key}).

%% -----------------------------------------------------------------------------
%% @doc
%%  deletes a game from the database asynchronously
%%
%%  will reply {tag (), ok} to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
delete_game (From, Key) ->
    gen_server:cast (service_worker:select_pid (game_worker),
                     {delete_game, From, Key}).
%% -----------------------------------------------------------------------------
%% @doc
%%  updates a game from the database asynchronously
%%
%%  will reply {tag (), {ok, GameKey :: Integer()}} to the calling process
%% in case of success
%% @end
%% -----------------------------------------------------------------------------
update_game(From, Game = #game{}) ->
    gen_server:cast(service_worker:select_pid(game_worker),
                    {new_game, From, Game}).

% -----------------------------------------------------------------------------
%% @doc
%%  join a player to a game from the database asynchronously
%%
%%  will reply {tag (), {ok, GameKey :: Integer()}} to the calling process
%% in case of success
%%  if country is already taken reply {tag (), {error, country_not_available}}
%% @end
%% -----------------------------------------------------------------------------
join_game(From, GameID, UserID, Country) ->
    gen_server:cast(service_worker:select_pid(game_worker),
                    {join_game, From, GameID, UserID, Country}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets a game players from the database and returns it asynchronously
%%
%%  will reply {tag (), {ok, #game{}}} to the calling process in case of success
%% @end
%% -----------------------------------------------------------------------------
get_game_players(From, GameID) ->
    gen_server:cast(service_worker:select_pid(game_worker),
                    {get_game_player, From, GameID}).



