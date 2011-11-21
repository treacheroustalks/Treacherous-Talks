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

-module(game_timer_phase_test).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").

-define (TEST_TIMEOUT, 3000).

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

apps () ->
    [datatypes, service, protobuffs, riakc, db, game].

app_started_setup () ->
    ?debugMsg ("starting apps:"),
    Response = [{App, application:start (App)} || App <- apps ()],
    ?debugMsg (io_lib:format ("~p", [Response])).

app_started_teardown (_) ->
    [application:stop (App) || App <- lists:reverse (apps ())].


%%------------------------------------------------------------------------------
%% @doc
%%  the top level test
%% @end
%%------------------------------------------------------------------------------
phase_timer_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_(),
      game_timer_create_tst_(),
      game_timer_state_tst_(),
      game_current_tst_(),
      game_timer_end_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = game_worker:ping () end].
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
             GameRecord = test_game3(),
             Game = sync_get(sync_new(GameRecord)),
             ?debugVal(Id = Game#game.id),
             ?debugVal(?assertEqual(waiting_phase, game_timer:current_state(Id))),
             ?debugVal(timer:sleep(100)),
             ?debugVal(game_timer:sync_event(Id, timeout)),
             ?assertEqual(order_phase, game_timer:current_state(Id)),

             game_timer:sync_event(Id, timeout),
             ?assertEqual(retreat_phase, game_timer:current_state(Id)),
             ?debugMsg("game timer state test end")
     end,
     fun() ->
             ?debugMsg("game timer reconfig test"),
             GameRecord = test_game3(),
             Game = sync_get(sync_new(GameRecord)),
             UpdatedGame = Game#game{description="RECONFIG",
                                     waiting_time = 1},
             ?assertEqual(waiting_phase, game_timer:current_state(Game#game.id)),
             game:reconfig_game(UpdatedGame),
             ?assertEqual(waiting_phase, game_timer:current_state(Game#game.id)),
             game_timer:sync_event(Game#game.id, timeout),
             ?assertEqual(order_phase, game_timer:current_state(Game#game.id)),
             ?assertEqual(UpdatedGame#game{status = ongoing},
                          game_timer:get_game_state(Game#game.id)),
             ?debugMsg("game timer reconfig test end")
     end].

%%--------------------------------------------------------------------
%% Tests current game state - changes state on sent events
%%--------------------------------------------------------------------
game_current_tst_() ->
    [fun() ->
             ?debugMsg("Current game update test----------"),
             GameRecord = test_game(),
             ?debugVal(Game = sync_get(sync_new(GameRecord))),
             ID = Game#game.id,

             ?assertEqual(waiting_phase, game_timer:current_state(ID)),

             %% timeout brings us to "started"
             game_timer:sync_event(ID, timeout),
             {ok, OrderCurrent} = game:get_current_game(ID),
             ?assertEqual(order_phase, OrderCurrent#game_current.current_phase),
             ?assertEqual({1901, spring},
                          OrderCurrent#game_current.year_season),

             game_timer:sync_event(ID, timeout),
             ?debugMsg("Process order - change phase to retreat"),
             {ok, RetreatCurrent} = game:get_current_game(ID),
             ?assertEqual(retreat_phase,
                          RetreatCurrent#game_current.current_phase),
             ?assertEqual({1901, spring},
                          RetreatCurrent#game_current.year_season),

             %% timeout brings us to build phase - but since it spring
             %% it will skip it and go back to order
             game_timer:sync_event(ID, timeout),
             ?debugMsg("Process retreat - skip build - change phase to order"),
             {ok, Current} = game:get_current_game(ID),
             ?assertEqual(order_phase, Current#game_current.current_phase),
             ?assertEqual({1901, fall}, Current#game_current.year_season),

             % after the current game has updated, we automatically go
             % to the next phase -> order_phase
             % process order phase
             game_timer:sync_event(ID, timeout),
             {ok, RetreatCurrent2} = game:get_current_game(ID),
             ?assertEqual(retreat_phase,
                          RetreatCurrent2#game_current.current_phase),
             ?assertEqual({1901, fall},
                          RetreatCurrent2#game_current.year_season),

             %% timeout to buildphase
             game_timer:sync_event(ID, timeout),
             {ok, BuildCurr} = game:get_current_game(ID),
             ?assertEqual(build_phase, BuildCurr#game_current.current_phase),

             game_timer:sync_event(ID, timeout),
             {ok, NewYearCurrent} = game:get_current_game(ID),
             ?assertEqual(order_phase,
                          NewYearCurrent#game_current.current_phase),
             ?assertEqual({1902, spring},
                          NewYearCurrent#game_current.year_season),
             ?debugMsg("Current game updates test end----------")
     end].

%%--------------------------------------------------------------------
%% Tests the ending of a game
%%--------------------------------------------------------------------
game_timer_end_tst_() ->
    [fun() ->
             ?debugMsg("finish game test----------"),
             GameRecord = test_game(),
             ?debugVal(Game = sync_get(sync_new(GameRecord))),
             ID = Game#game.id,
             ?debugVal(TimerPid = global:whereis_name({game_timer, ID})),
             ?assert(is_process_alive(TimerPid)),
             ?assertEqual(game_timer:sync_event(ID, timeout), {ok, order_phase}),
             ?assertEqual({ID, finished}, game_timer:stop(ID, finished)),
             
             ?assertEqual({ok, Game#game{status = finished}}, game:get_game(ID)),
             %% check that the game timer does not exist
             ?assertNot(is_process_alive(TimerPid)),
             ?assertEqual(undefined, global:whereis_name({game_timer, ID}))
     end].


%%--------------------------------------------------------------------
%% Helper functions for creating and getting games
%%--------------------------------------------------------------------
sync_new(Game=#game{}) ->
    {ok, Id} = game:new_game(Game),
    Id.

sync_get(ID) ->
    {ok, Game} = game:get_game(ID),
    Game.
