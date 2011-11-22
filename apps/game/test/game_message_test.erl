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
-module(game_message_test).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").
-include_lib ("datatypes/include/message.hrl").
-include_lib ("datatypes/include/push_event.hrl").


-export([receive_push_event/2, receive_init/0]).
-define (TEST_TIMEOUT, 3000).

apps () ->
    [datatypes, service, protobuffs, riakc, db, game, message].

app_started_setup () ->
    ?debugMsg ("starting apps:"),
    Response = [{App, application:start (App)} || App <- apps ()],
    meck:new(controller),
    meck:expect(controller, push_event,
                fun(UserID, Event) ->
                        game_msg ! {controller_push_event, {UserID, Event}},
                         ok end),
    ?debugMsg (io_lib:format ("~p", [Response])).

app_started_teardown (_) ->
    [application:stop (App) || App <- lists:reverse (apps ())],
    meck:unload(controller).


%%------------------------------------------------------------------------------
%% @doc
%%  the top level test
%% @end
%%------------------------------------------------------------------------------
move_get_put_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_(),
      get_target_players_tst_(),
      game_msg_send_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = game_worker:ping () end].

test_game (Press) ->
    #game{creator_id=123,
          name="game name",
          description="lorem ipsum dolor sit amet",
          press = Press,
          order_phase = 12*60,
          retreat_phase = 12*60,
          build_phase = 12*60,
          password="pass",
          waiting_time = 50*60}.

%%------------------------------------------------------------------------------
%% Tests the get game state functionality
%%------------------------------------------------------------------------------

get_target_players_tst_() ->
    [fun() ->
    Expected = [{game_user,4444,england},
                                       {game_user,3333,italy},
                                       {game_user,1111,austria}],
    Actual = game_worker:get_target_players([austria,italy,england],[
                #game_user{id=1111,country = austria},
                #game_user{id=2222,country = germany},
                #game_user{id=3333,country = italy},
                #game_user{id=4444,country = england}
            ],[]),
    ?assertEqual(Expected,Actual)
    end].

%% @TODO write test for game_msg
game_msg_send_tst_() ->
    [fun() ->
             ?debugMsg("Ensure a in-game message is correctly dilivered to controller"),
             ?debugMsg("send game message test to several users white press"),
             GameRecord = test_game(white),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             JoinResult = game:join_game(Game#game.id, 1111, england),
             ?assertEqual({ok, Game#game.id}, JoinResult),
             JoinResult2 = game:join_game(Game#game.id, 2222, germany),
             ?assertEqual({ok, Game#game.id}, JoinResult2),
             JoinResult3 = game:join_game(Game#game.id, 3333, france),
             ?assertEqual({ok, Game#game.id}, JoinResult3),
             UserIDs = [2222,3333],
             FromCountries = [england, england],

             game_timer:sync_event(Game#game.id, timeout),
             timer:sleep(50),
             GMsg = #game_message{content = "test send game message", from_id= 1111,
                                  game_id = Game#game.id},

             {Result1, Result2} = sync_game_msg(GMsg, [germany, france]),
             ?assertEqual([], UserIDs -- Result1),
             ?assertEqual([], FromCountries -- Result2)
    end,
     fun() ->
             ?debugMsg("send game message when game is not started"),
             GameRecord = test_game(white),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             JoinResult = game:join_game(Game#game.id, 1111, england),
             ?assertEqual({ok, Game#game.id}, JoinResult),
             JoinResult2 = game:join_game(Game#game.id, 2222, germany),
             ?assertEqual({ok, Game#game.id}, JoinResult2),
             JoinResult3 = game:join_game(Game#game.id, 3333, france),
             ?assertEqual({ok, Game#game.id}, JoinResult3),

             timer:sleep(50),
             GMsg = #game_message{content = "test send game message", from_id= 1111,
                                  game_id = Game#game.id},
             Result = game:game_msg(GMsg, [germany, france]),
             ?assertEqual({error,game_phase_not_ongoing}, Result)
    end,
     fun() ->
             ?debugMsg("Ensure a in-game message is correctly dilivered to controller"),
             ?debugMsg("send game message to several users grey press"),
             GameRecord = test_game(grey),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             JoinResult = game:join_game(Game#game.id, 1111, england),
             ?assertEqual({ok, Game#game.id}, JoinResult),
             JoinResult2 = game:join_game(Game#game.id, 2222, germany),
             ?assertEqual({ok, Game#game.id}, JoinResult2),
             JoinResult3 = game:join_game(Game#game.id, 3333, france),
             ?assertEqual({ok, Game#game.id}, JoinResult3),
             UserIDs = [2222,3333],
             FromCountries =[unknown, unknown],

             game_timer:sync_event(Game#game.id, timeout),
             timer:sleep(50),
             GMsg = #game_message{content = "test send game message", from_id= 1111,
                                  game_id = Game#game.id},

             {Result1, Result2} = sync_game_msg(GMsg, [germany, france]),
             ?assertEqual([], UserIDs -- Result1),
             ?assertEqual([], FromCountries -- Result2)
    end].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%% @doc
%%   synchronious send game message
%%   Since message:game_msg is a gen_server cast, we need to meck up controller
%%   function which being called when game messages are logged.
%%   this function sends message to message app and meck controller:push_event
%%   to get the reponse from the function's arguments.
%%   it returns the userid of the receiver of the message and the from country
%%   of the sender.
%% @end
%%------------------------------------------------------------------------------
sync_game_msg(GMsg, Countries) ->
    Pid = spawn(?MODULE, receive_init, []),
    register(game_msg, Pid),

    {ok, _GameID} = game:game_msg(GMsg, Countries),
    receive
    after
        3000 ->
            Pid!{self(), get_user_ids},
            receive
                {ok, UserIDs, FromCountries} ->
                    {UserIDs, FromCountries}
            end
    end.

receive_init()->
    receive_push_event([], []).

receive_push_event(UserIDs, FromCountries) ->
    receive
        {controller_push_event,
         {UserID, #push_event{data = GMsg}}} ->
            FromCountry = GMsg#game_message.from_country,
            receive_push_event([UserID | UserIDs], [FromCountry | FromCountries]);
        {Pid, get_user_ids} ->
            unregister(game_msg),
            Pid ! {ok, UserIDs, FromCountries}
    end.

sync_new(Game=#game{}) ->
    {ok, Id} = game:new_game(Game),
    Id.

sync_get(ID) ->
    {ok, Game} = game:get_game(ID),
    Game.
