-module(game_test).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").

-define (TEST_TIMEOUT, 3000).

apps () ->
    [datatypes, service, protobuffs, riakc, db, game].

app_started_setup () ->
    [application:start (App) || App <- apps ()].

app_started_teardown (_) ->
    [application:stop (App) || App <- lists:reverse (apps ())].

test_game () ->
    #game{creator_id=123,
          name="game name",
          description="lorem ipsum dolor sit amet",
          press = black_press,
          order_phase = 12*60,
          retreat_phase = 12*60,
          build_phase = 12*60,
          password="pass",
          waiting_time = 48*60}.

%%------------------------------------------------------------------------------
%% @doc
%%  check, that the test_game record is built correctly. if not, it would throw
%%  an exception
%% @end
%%------------------------------------------------------------------------------
test_game_test () ->
    ?debugVal (test_game ()).

%%------------------------------------------------------------------------------
%% @doc
%%  the top level test
%% @end
%%------------------------------------------------------------------------------
game_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_ (),
      new_get_game_tst_ (),
      delete_game_tst_ (),
      game_timer_create_tst_(),
      game_timer_state_tst_()]}.

%%------------------------------------------------------------------------------
%% @doc
%%  "testlet", pinging the worker
%% @end
%%------------------------------------------------------------------------------

ping_tst_ () ->
    [fun () -> {pong, _Pid} = game_worker:ping () end].

%%------------------------------------------------------------------------------
%% @doc
%%  creates and reads a game
%% @end
%%------------------------------------------------------------------------------
new_get_game_tst_ () ->
    [fun () ->
             OrigGame = test_game (),
             Key = sync_new (OrigGame),
             OrigGame = sync_get (Key),
             Key
     end].

%%------------------------------------------------------------------------------
%% @doc
%%  creates a game, deletes it again and makes sure, it's gone
%% @end
%%------------------------------------------------------------------------------
delete_game_tst_ () ->
    [fun () ->
             [Get]=new_get_game_tst_ (),
             Key=Get (),
             sync_delete (Key),
             ?assertException (error, _, sync_get (Key))
     end].

%%--------------------------------------------------------------------
%% Tests game timer - creation of a new game timer
%%--------------------------------------------------------------------
game_timer_create_tst_() ->
    [fun() ->
             Game = test_game(),
             ?assertMatch({ok, _Pid}, game_timer_sup:create_timer(Game))
     end].

%%--------------------------------------------------------------------
%% Tests game timer - changes state on sent events
%%--------------------------------------------------------------------
game_timer_state_tst_ () ->
    [fun() ->
             Game = test_game(),
             % create a new game timer
             {ok, TimerPid} = game_timer_sup:create_timer(Game),
             % assert that we have entered the waiting phase
             ?assertEqual(waiting_phase, game_timer:current_state(TimerPid)),
             % trigger an event to go to the next phase (only for testing)
             game_timer:event(TimerPid, event),
             % assert that we are in the order phase
             ?assertEqual(order_phase, game_timer:current_state(TimerPid)),
             game_timer:event(TimerPid, event),
             ?assertEqual(retreat_phase, game_timer:current_state(TimerPid))
     end].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
sync_new (Game=#game{}) ->
    game:new_game ({self (), new_game}, test_game ()),
    receive
        {new_game, {ok, Key}} ->
            ?debugMsg (io_lib:format ("~p",[{created_new, Key}])),
            Key;
        {new_game, Other} ->
            erlang:error ({error, {{received, Other}, {expected, {ok, key}}}})
    after ?TEST_TIMEOUT ->
            erlang:error ({error, {no_asynch_answer, ?FILE, ?LINE}})
    end.

sync_get (ID) ->
    ok = game:get_game ({self (), get_game}, ID),
    receive
        {get_game, {ok, Game}} ->
            ?debugMsg ("received game"),
            Game;
        {get_game, Other} ->
            erlang:error ({error, {{received, Other}, {expected, {ok, key}}}})
    after ?TEST_TIMEOUT ->
            erlang:error ({error, {no_asynch_answer, ?FILE, ?LINE}})
    end.

sync_delete (ID) ->
    ok = game:delete_game ({self (), delete_game}, ID),
    receive
        {delete_game, ok} ->
            ?debugMsg ("deleted game"),
            ok;
        {delete_game, Other} ->
            erlang:error ({error, {{received, Other}, {expected, ok}}})
    after ?TEST_TIMEOUT ->
            erlang:error ({error, {no_asynch_answer, ?FILE, ?LINE}})
    end.
