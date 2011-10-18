-module (game).

-export ([new_game/2]).

-include_lib ("datatypes/include/game.hrl").

%% @doc creates a new game asynchronously
%% look at {@link game_worker:direct_reply/2} for the response specification.
new_game (From={_Pid, _Tag}, Game=#game{}) ->
    gen_server:cast (service_worker:select_pid(game_worker),
                     {new_game, From, Game}).
