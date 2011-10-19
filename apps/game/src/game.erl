-module (game).

-type tag () :: {pid (), any ()}.

-export ([new_game/2,
          get_game/2,
          delete_game/2]).

-include_lib ("datatypes/include/game.hrl").

%% -----------------------------------------------------------------------------
%% @doc 
%%  creates a new game asynchronously.
%%
%%  will reply {tag (), {ok, GameKey :: binary ()}} to the calling process 
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
