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
          waiting_time = 50*60}.

test_game2 () ->
    #game{ id = 111222,
           creator_id=123,
           name="game name",
           description="lorem ipsum dolor sit amet",
           press = black_press,
           order_phase = 100,
           retreat_phase = 100,
           build_phase = 100,
           password="pass",
           waiting_time = 100}.

test_game3 () ->
    #game{ creator_id=123,
           name="game name",
           description="this is a long game!",
           press = black_press,
           order_phase = 100,
           retreat_phase = 100,
           build_phase = 100,
           password="pass",
           waiting_time = 5}.

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
             ?debugMsg("game timer state test start"),
             Game = test_game2(),
             Id = Game#game.id,
             % create a new game timer
             game_timer_sup:create_timer(Game),
             ?assertEqual(waiting_phase, game_timer:current_state(Id)),
             % assert that we have entered the waiting phase
             game_timer:event(Id, timeout),
             ?assertEqual(order_phase, game_timer:current_state(Id)),
             % trigger an event to go to the next phase (only for testing)
             % assert that we are in the order phase
             game_timer:event(Id, timeout),
             ?assertEqual(retreat_phase, game_timer:current_state(Id)),
             game_timer:event(Id, timeout),
             ?assertEqual(build_phase, game_timer:current_state(Id)),
             ?debugMsg("game timer state test end")
     end,
     fun() ->
             ?debugMsg("game timer reconfig test"),
             GameRecord = test_game3(),
             Game = sync_get(sync_new(GameRecord)),
             UpdatedGame = Game#game{description="RECONFIG",
                                     waiting_time = 1},
             ?debugVal(UpdatedGame),
             ?assertEqual(waiting_phase, game_timer:current_state(Game#game.id)),
             game:reconfig_game(UpdatedGame),
             ?assertEqual(waiting_phase, game_timer:current_state(Game#game.id)),
             timer:sleep(20),
             game_timer:event(Game#game.id, timeout),
             ?assertEqual(order_phase, game_timer:current_state(Game#game.id)),
             ?assertEqual(UpdatedGame#game{status = ongoing},
                          game_timer:get_game_state(Game#game.id)),
             ?debugMsg("game timer reconfig test end")
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
              game:reconfig_game(UpdatedGame),
              timer:sleep(50),
              %% Now the game should have changed in the DB to have
              %% status = ongoing, change that before assert
              ?assertEqual(UpdatedGame, sync_get(Game#game.id)),
              ?debugMsg("Update game test end")
      end].

%%------------------------------------------------------------------------------
%% Tests the join game functionality
%%------------------------------------------------------------------------------
join_game_tst_() ->
    [fun() ->
              ?debugMsg("join game test"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              JoinResult = game:join_game(Game#game.id, 1122, england),
              ?assertEqual({ok, Game#game.id}, JoinResult),
              timer:sleep(50),
              GamePlayers = sync_get_game_player (Game#game.id),
              ?assertEqual(1, length(GamePlayers#game_player.players)),
              ?debugMsg("join game test end")
      end,
     fun() ->
              ?debugMsg("join game test when country already taken"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              game:join_game(Game#game.id, 1122, england),
              timer:sleep(50),
              JoinResult = game:join_game(Game#game.id, 221122, england),
              ?assertEqual({error, country_not_available}, JoinResult),
              ?debugMsg("join game test end")
      end,
     fun() ->
              ?debugMsg("join game test when user is already in the game"),
              GameRecord = test_game(),
              % Create a new Game
              Game = sync_get(sync_new(GameRecord)),
              % join new player with id=1122 and country=england
              game:join_game(Game#game.id, 1122, england),
              timer:sleep(50),
              JoinResult = game:join_game(Game#game.id, 1122, austria),
              ?assertEqual({error, user_already_joined}, JoinResult),
              ?debugMsg("join game test end")
      end,
    fun() ->
            ?debugMsg("game join proc start and end"),
            {ok, GameId} = game:new_game(test_game()),
            Game = sync_get(GameId),
            JoinProcPid = game_join_proc_map:get_pid(GameId),

            % a join proc exists for the game after creating the game
            ?assertEqual(true, game_join_proc:is_alive(JoinProcPid)),
            game:phase_change(Game, started),

            % block only until the join process is gone.
            % If the test times out, the proc probably didn't die within 5s
            % and suggests something's wrong.
            MonitorRef = monitor(process, JoinProcPid),
            receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end,
            % no join proc exists for the game when the game has started
            ?assertEqual(false, game_join_proc:is_alive(JoinProcPid)),

            % no entry exists for the game in the game id -> pid DB
            ?assertEqual(none, game_join_proc_map:get_pid(GameId))
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
              game:join_game(Game#game.id, UserID, Country),
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
              ?assertEqual(user_not_playing_this_game, Reply),
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
              game:join_game(Game#game.id, UserID, Country),
              timer:sleep(50),
              Reply = sync_get_game_state (Game#game.id, UserID),
              ?assertEqual(game_not_waiting, Reply),
              ?debugMsg("Game is not in waiting phase"),
              ?debugMsg("get game state test end")
      end,
     fun() ->
             ?debugMsg("Test joining a game which doesn't exist"),
             sync_delete(1234), % ensure it doesn't exist
             ?assertEqual({error, notfound},
                          game:join_game(1234, 1122, england))
     end
     ].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
sync_new(Game=#game{}) ->
    {ok, Id} = game:new_game(Game),
    Id.

sync_get(ID) ->
    {ok, Game} = game:get_game(ID),
    Game.

sync_get_game_player(ID) ->
    {ok, GamePlayer} = game:get_game_players(ID),
    GamePlayer.

sync_join_game_player(GameID, UserID, Country) ->
    case game:join_game(GameID, UserID, Country) of
        {ok, GamePlayer} ->
            GamePlayer;
        {error, country_not_available} ->
            country_not_available
    end.

sync_get_game_state(GameID, UserID) ->
    case game:get_game_state(GameID, UserID) of
        {ok, GameOverview} ->
            GameOverview;
        {error, user_not_playing_this_game} ->
            user_not_playing_this_game;
        {error, game_not_waiting} ->
            game_not_waiting;
        Other ->
            erlang:error ({error, {{received, Other}, {expected, {ok, key}}}})
    end.

sync_delete(ID) ->
    case game:delete_game(ID) of
        ok ->
            ?debugMsg ("deleted game"),
            ok;
        Other ->
            erlang:error ({error, {{received, Other}, {expected, ok}}})
    end.