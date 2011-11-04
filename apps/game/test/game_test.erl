-module(game_test).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").

-define (TEST_TIMEOUT, 3000).

apps () ->
    [datatypes, service, protobuffs, riakc, db, game].

app_started_setup () ->
    ?debugMsg ("starting apps:"),
    Response = [{App, application:start (App)} || App <- apps ()],
    ?debugMsg (io_lib:format ("~p", [Response])).

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
      game_timer_state_tst_(),
      game_update_tst_(),
      join_game_tst_(),
      get_game_state_tst_()
     ]}.

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
             % OrigGame is actually updated with an ID
             OrigGameWithId = OrigGame#game{id = Key},
             OrigGameWithId = sync_get (Key),
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
%% Tests the game update functionality
%%------------------------------------------------------------------------------
game_update_tst_() ->
     [fun() ->
              ?debugMsg("Update game test"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % Create a copy of Game with a new description
              UpdatedGame = Game#game{description = "Updated game"},
              % Update the game with the same id as Game to UpdatedGame
              game:reconfig_game({self(), update_test}, UpdatedGame),
              timer:sleep(50),
              ?assertEqual(UpdatedGame, sync_get(Game#game.id)),
              ?debugMsg("Update game test end")
      end].

%%------------------------------------------------------------------------------
%% Tests the goin game functionality
%%------------------------------------------------------------------------------
join_game_tst_() ->
    [fun() ->
              ?debugMsg("join game test"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              game:join_game({self(), join_game_test},
                             Game#game.id, 1122, england),
              timer:sleep(50),
              GP = sync_get_game_player (Game#game.id),
              ?assertEqual(1, length(GP#game_player.players)),
              ?debugMsg("join game test end")
      end,
     fun() ->
              ?debugMsg("join game test when country already taken"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              game:join_game({self(), join_game_test},
                             Game#game.id, 1122, england),
              timer:sleep(50),
              Msg = sync_join_game_player (Game#game.id, 221122, england),
              ?assertEqual(country_not_available, Msg),
              ?debugMsg("join game test end")
      end].
%%------------------------------------------------------------------------------
%% Tests the get game state functionality
%%------------------------------------------------------------------------------
get_game_state_tst_ () ->
    [fun() ->
              ?debugMsg("start get game state test ..."),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              UserID = 1122,
              Country = england,
              game:join_game({self(), join_game_test},
                             Game#game.id, UserID, Country),
              timer:sleep(50),
              GOV = sync_get_game_state (Game#game.id, UserID),
              ?assertEqual(Country, GOV#game_overview.country),
              ?debugMsg("game state retrieved")
      end,
    fun() ->
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              UserID = 1122,
              %Country = england,
              %game:join_game({self(), join_game_test},
              %               Game#game.id, UserID, Country),
              timer:sleep(50),
              Reply = sync_get_game_state (Game#game.id, UserID),
              ?assertEqual(user_not_play_this_game, Reply),
              ?debugMsg("User does not play this game")
      end,
    fun() ->
              GameRecord = test_game(),
              GameRecord2 = GameRecord#game{status = ongoing},
              % Create a new Game
              Game = sync_get(sync_new(GameRecord2)),
              % join new player with id=1122 and country=england
              UserID = 1122,
              Country = england,
              game:join_game({self(), join_game_test},
                             Game#game.id, UserID, Country),
              timer:sleep(50),
              Reply = sync_get_game_state (Game#game.id, UserID),
              ?assertEqual(game_not_waiting, Reply),
              ?debugMsg("Game is not in waiting phase"),
              ?debugMsg("get game state test end")
      end
     ].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
sync_new (Game=#game{}) ->
    game:new_game ({self (), new_game}, Game),
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

sync_get_game_player (ID) ->
    ok = game:get_game_players ({self (), get_game_player}, ID),
    receive
        {get_game_player, {ok, GamePlayer}} ->
            ?debugMsg ("received game player"),
            GamePlayer;
        {get_game_player, Other} ->
            erlang:error ({error, {{received, Other}, {expected, {ok, key}}}})
    after ?TEST_TIMEOUT ->
            erlang:error ({error, {no_asynch_answer, ?FILE, ?LINE}})
    end.

sync_join_game_player (GameID, UserID, Country) ->
    ok = game:join_game ({self (), sync_join_game}, GameID, UserID, Country),
    receive
        {sync_join_game, {ok, GamePlayer}} ->
            ?debugMsg ("received game player"),
            GamePlayer;
        {sync_join_game, {error, country_not_available}} ->
            ?debugMsg ("join game country not available"),
            country_not_available;
        {sync_join_game, Other} ->
            erlang:error ({error, {{received, Other}, {expected, {ok, key}}}})
    after ?TEST_TIMEOUT ->
            erlang:error ({error, {no_asynch_answer, ?FILE, ?LINE}})
    end.

sync_get_game_state (GameID, UserID) ->
    ok = game:get_game_state ({self (), sync_get_game_state}, GameID, UserID),
    receive
        {sync_get_game_state, {ok, GameOverview}} ->
            GameOverview;
        {sync_get_game_state, {error, user_not_play_this_game}} ->
            user_not_play_this_game;
        {sync_get_game_state, {error, game_not_waiting}} ->
            game_not_waiting;
        {sync_get_game_state, Other} ->
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
