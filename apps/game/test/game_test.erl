-module(game_test).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").

apps () ->
    [datatypes, service, protobuffs, riakc, db, game].

app_started_setup () ->
    [application:start (App) || App <- apps ()].

app_started_teardown (_) ->
    [application:stop (App) || App <- lists:reverse (apps ())].

game_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_ (), new_game_tst_ ()
      ]}.

ping_tst_ () ->
    [fun () -> {pong, _Pid} = game_worker:ping () end].

new_game_tst_ () ->
    [fun () ->
             ?debugVal (game:new_game ({self (), "some_tag"},
                                               #game{name="test-game"})),
             receive
                 {"some_tag", {ok, GameKey}} -> ?debugVal (GameKey)
             after 3000 ->
                     erlang:error ({error, no_asynch_answer})
             end
     end].
