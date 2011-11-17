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

% input and output for test game order
test_order_list() ->
    {[{convoy,fleet,north_sea,army,liverpool,norwegian_sea},
      {convoy,fleet,north_sea,army,rome,norwegian_sea},
      {move,army,london,norwegian_sea,any_coast},
      {move,army,edinburg,norwegian_sea,north_coast},
      {move,any_unit,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,any_coast},
      {hold, army, paris},
      {hold, fleet, kiel},
      {support_move, army, london, army, berlin, munich, any_coast},
      {support_hold, army, london, army, moscow},
      {support_hold, army, london, army, bohemia},
      {build, army, stockholm, any_coast},
      {remove, army, munich}
     ],
     %% expected output
     [{destroy,{army,england},munich},
      {build,{army,england},stockholm},
      {support,{army,england},london,{hold,{army,russia},moscow}},
      {support,{army,england},london,{move,{army,germany},berlin,munich}},
      {hold,{fleet,england},kiel},
      {hold,{army,england},paris},
      {move,{army,england},london,norwegian_sea},
      {move,{any_unit,england},london,norwegian_sea},
      {move,{army,england},edinburg,norwegian_sea},
      {move,{army,england},london,norwegian_sea},
      {convoy,{fleet,england},north_sea,{army,italy},rome,norwegian_sea},
      {convoy,{fleet,england},north_sea,{army,england},liverpool,norwegian_sea}]
    }.

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
     [ping_tst_(),
      new_get_game_tst_(),
      delete_game_tst_(),
      game_update_tst_(),
      join_game_tst_(),
      get_game_overview_tst_(),
      translate_game_order_tst_()
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
             OrigGame = test_game(),
             Key = sync_new(OrigGame),
             % OrigGame is actually updated with an ID
             OrigGameWithId = OrigGame#game{id = Key},
             OrigGameWithId = sync_get (Key)
     end,
     fun () ->
             OrigGame = test_game (),
             Key = sync_new(OrigGame),
             Game = sync_get(Key),
             {ok, Keys} = game:get_keys_by_idx(#game.status, Game#game.status),
             ?assertEqual(true, lists:member(Key, Keys))
     end].

%%------------------------------------------------------------------------------
%% @doc
%%  creates a game, deletes it again and makes sure, it's gone
%% @end
%%------------------------------------------------------------------------------
delete_game_tst_ () ->
    [fun () ->
             OrigGame = test_game(),
             Key = sync_new(OrigGame),
             sync_delete(Key),
             ?assertException(error, _, sync_get (Key))
     end,
     fun () ->
             % create a game
             OrigGame = test_game (),
             Key = sync_new(OrigGame),
             Game = sync_get(Key),

             % prove that we can find it
             {ok, Keys} = game:get_keys_by_idx(#game.status, Game#game.status),
             ?assertEqual(true, lists:member(Key, Keys)),

             % delete it
             sync_delete(Key),

             % prove that we don't find it
             {ok, Keys2} = game:get_keys_by_idx(#game.status, Game#game.status),
             ?assertEqual(false, lists:member(Key, Keys2))
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
      end,
      fun () ->
             % create a game
             OrigGame = test_game (),
             Key = sync_new(OrigGame),
             Game = sync_get(Key),

             % prove that we can find it
             {ok, Keys} = game:get_keys_by_idx(#game.press, Game#game.press),
             ?assertEqual(true, lists:member(Key, Keys)),

             % update it
             ModifiedGame = Game#game{press = white_press},
             game:reconfig_game(ModifiedGame),
             timer:sleep(50),

             % prove that we find it
             {ok, Keys2} = game:get_keys_by_idx(#game.press, white_press),
             ?assertEqual(true, lists:member(Key, Keys2)),

             % prove that we don't find it
             {ok, Keys3} = game:get_keys_by_idx(#game.press, black_press),
             ?assertEqual(false, lists:member(Key, Keys3))
      end,
      fun () ->
             % create a game
             OrigGame = test_game (),
             Key = sync_new(OrigGame),
             Game = sync_get(Key),

             % prove that we can find it
             {ok, Keys} = game:get_keys_by_idx(#game.status, waiting),
             ?assertEqual(true, lists:member(Key, Keys)),

             % game changes status
             game_timer:sync_event(Game#game.id, timeout),

             % prove that we find it
             {ok, Keys2} = game:get_keys_by_idx(#game.status, ongoing),
             ?assertEqual(true, lists:member(Key, Keys2)),

             % prove that we don't find it
             {ok, Keys3} = game:get_keys_by_idx(#game.status, waiting),
             ?assertEqual(false, lists:member(Key, Keys3))
      end,
      fun() ->
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
%% test translate game order functionality
%%------------------------------------------------------------------------------
translate_game_order_tst_() ->
    [fun() ->
             ?debugMsg("translate game order test"),
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             JoinResult = game:join_game(Game#game.id, 1122, england),
             ?assertEqual({ok, Game#game.id}, JoinResult),
             {GameOrderList, ExpectedOutput} = test_order_list(),

             % start the game
             game_timer:sync_event(Game#game.id, timeout),

             Result = game_utils:translate_game_order(Game#game.id,
                                                       GameOrderList,england),
             ?assertEqual(ExpectedOutput, Result),
             ?debugMsg("successful translate game order test")
      end
     ].

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
             game_timer:sync_event(Game#game.id, timeout),
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
%% Tests the get game overview functionality
%%------------------------------------------------------------------------------
get_game_overview_tst_ () ->
    [fun() ->
             ?debugMsg("Get game overview test. Case: VALID"),
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             UserID = 1122,
             Country = england,
             game:join_game(Game#game.id, UserID, Country),
             % start the game
             game_timer:sync_event(Game#game.id, timeout),
             GOV = sync_get_game_overview (Game#game.id, UserID),
             ?debugMsg("1Game Overview -------------------------->>"),
             ?debugVal(GOV),
             ?assertEqual(Country, GOV#game_overview.country),
             ?debugMsg("game state retrieved")
     end,
     fun() ->
             ?debugMsg("Get game overview test. Case: INVALID USER"),
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             UserID = 11223,
             % start the game
             game_timer:sync_event(Game#game.id, timeout),
             Reply = sync_get_game_overview (Game#game.id, UserID),
             ?debugMsg("2Game Overview -------------------------->>"),
             ?debugVal(Reply),
             ?assertEqual(user_not_playing_this_game, Reply),
             ?debugMsg("User does not play this game")
     end,
     fun() ->
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             UserID = 1122,
             Country = england,
             game:join_game(Game#game.id, UserID, Country),
             % start the game
             game_timer:sync_event(Game#game.id, timeout),
             Reply = sync_get_game_overview (Game#game.id, UserID),
             ?debugMsg("3Game Overview -------------------------->>"),
             ?debugVal(Reply),
             StandardMap = digraph_io:to_erlang_term(
                             map_data:create(standard_game)),
             StoredMap = Reply#game_overview.map,
             ?assertEqual(StandardMap, StoredMap),
             ?debugMsg("Game phase changed, and map is the same"),
             ?debugMsg("get game state test end")
     end,
     fun() ->
             ?debugMsg("Test joining a game which doesn't exist"),
             sync_delete(1234), % ensure it doesn't exist
             ?assertEqual({error, notfound},
                          game:join_game(1234, 1122, england))
     end].

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

sync_get_game_overview(GameID, UserID) ->
    case ?debugVal(game:get_game_overview(GameID, UserID)) of
        {ok, GameOverview} ->
            ?debugMsg("Getting game overview ---------------------->>"),
            ?debugVal(GameOverview),
            GameOverview;
        {error, Error} ->
            Error;
        _Other ->
            unknown_error
    end.

sync_delete(ID) ->
    case game:delete_game(ID) of
        ok ->
            ?debugMsg ("deleted game"),
            ok;
        Other ->
            erlang:error ({error, {{received, Other}, {expected, ok}}})
    end.
