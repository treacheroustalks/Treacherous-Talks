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
-include_lib ("datatypes/include/bucket.hrl").

-export([receive_push_event/2, receive_init/0]).
-define (TEST_TIMEOUT, 3000).

apps () ->
    [datatypes, service, protobuffs, riakc, db, game, message, utils].

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
game_messaging_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_(),
      get_target_players_tst_(),
      game_msg_send_tst_(),
      mod_msg_success_tst_(),
      user_msg_fail_tst_(),
      operator_get_game_msg_tst_(),
      get_game_msg_tree_tst_()
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
mod_msg_success_tst_() ->
    fun() ->
            GameRecord = test_game(grey),
            % Create a new Game
            Game = sync_get(sync_new(GameRecord)),
            % join players and countries
            JoinResult = game:join_game(Game#game.id, 1111, england),
            ?assertEqual({ok, Game#game.id}, JoinResult),
            JoinResult2 = game:join_game(Game#game.id, 2222, germany),
            ?assertEqual({ok, Game#game.id}, JoinResult2),
            JoinResult3 = game:join_game(Game#game.id, 3333, france),
            ?assertEqual({ok, Game#game.id}, JoinResult3),
            game_timer:sync_event(Game#game.id, timeout),

            GMsg = #game_message{content = "test moderator game message",
                                 from_id= 1357,
                                 game_id = Game#game.id},
            %?debugVal(_Result = game:game_msg(GMsg, Countries, moderator)),
            {UserIDs, Senders} = sync_game_msg(GMsg,
                                               [germany, france], moderator),
            sync_delete(Game#game.id),
            ?assert(lists:member(2222, UserIDs)),
            ?assert(lists:member(3333, UserIDs)),
            ?assertEqual([moderator, moderator], Senders)
    end.

user_msg_fail_tst_() ->
    fun() ->
            GameRecord = test_game(grey),
            % Create a new Game
            Game = sync_get(sync_new(GameRecord)),
            % join players and countries
            JoinResult = game:join_game(Game#game.id, 1111, england),
            ?assertEqual({ok, Game#game.id}, JoinResult),
            JoinResult2 = game:join_game(Game#game.id, 2222, germany),
            ?assertEqual({ok, Game#game.id}, JoinResult2),
            JoinResult3 = game:join_game(Game#game.id, 3333, france),
            ?assertEqual({ok, Game#game.id}, JoinResult3),
            game_timer:sync_event(Game#game.id, timeout),

            % the sender is not a game player!
            GMsg = #game_message{content = "test user game message", from_id= 1357,
                                 game_id = Game#game.id},
            Res = game:game_msg(GMsg, [germany, france], user),
            sync_delete(Game#game.id),
            ?assertEqual({error, user_not_playing_this_game}, Res)
    end.

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

             {Result1, Result2} = sync_game_msg(GMsg, [germany, france], user),
             sync_delete(Game#game.id),
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
             Result = game:game_msg(GMsg, [germany, france], user),
             sync_delete(Game#game.id),
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
             ?debugMsg("sync_game_msg"),
             {Result1, Result2} = sync_game_msg(GMsg, [germany, france], user),
             sync_delete(Game#game.id),
             ?assertEqual([], UserIDs -- Result1),
             ?assertEqual([], FromCountries -- Result2)
    end].

get_game_msg_tree_tst_() ->
    [fun() ->
        Msg1 = #game_message{game_id=7777, content="msg1", year = 1901,season=fall,phase=order,date_created=1,sender_country=england},
        Msg2 = #game_message{game_id=7777, content="msg2", year = 1902,season=spring,phase=build,date_created=2,sender_country=germany},
        Msg3 = #game_message{game_id=7777, content="msg3", year = 1902,season=fall,phase=retreat,date_created=3,sender_country=england},
        Msg4 = #game_message{game_id=7777, content="msg4", year = 1904,season=spring,phase=retreat,date_created=4,sender_country=russia},
        Msg5 = #game_message{game_id=7777, content="msg5", year = 1905,season=spring,phase=order,date_created=5,sender_country=austria},
        put_game_msg(99991, Msg1),
        put_game_msg(99992, Msg2),
        put_game_msg(99993, Msg3),
        put_game_msg(99994, Msg4),
        put_game_msg(99995, Msg5),
        {ok, Actual} = game_utils:get_game_msg_tree(7777),
        io:format(user,"~p~n",[Actual]),
        del_game_msg(99991),
        del_game_msg(99992),
        del_game_msg(99993),
        del_game_msg(99994),
        del_game_msg(99995),
        Expected = [{1901,[{{fall,order},[england]}]},
                {1902,[{{spring,build},[germany]},{{fall,retreat},[england]}]},
                {1904,[{{spring,retreat},[russia]}]},
                {1905,[{{spring,order},[austria]}]}],
        ?assertEqual(Expected, Actual)
    end].

operator_get_game_msg_tst_() ->
    [fun() ->
        Msg1 = #game_message{game_id=7777, content="msg1", year = 1902,season=fall,phase=order,date_created=1,sender_country=england},
        Msg2 = #game_message{game_id=7777, content="msg2", year = 1902,season=fall,phase=order,date_created=2,sender_country=england},
        Msg3 = #game_message{game_id=7777, content="msg3", year = 1902,season=fall,phase=retreat,date_created=3,sender_country=england},
        Msg4 = #game_message{game_id=7777, content="msg4", year = 1904,season=spring,phase=retreat,date_created=4,sender_country=russia},
        Msg5 = #game_message{game_id=7777, content="msg5", year = 1905,season=spring,phase=order,date_created=5,sender_country=austria},
        put_game_msg(99991, Msg1),
        put_game_msg(99992, Msg2),
        put_game_msg(99993, Msg3),
        put_game_msg(99994, Msg4),
        put_game_msg(99995, Msg5),
        Key = "7777-1902-fall-order_phase-england",
        BinID = list_to_binary(Key),
        DBGameOrderObj = db_obj:create (?B_GAME_ORDER, BinID, #game_order{order_list=[move,support,convoy,hold]}),
        db:put (DBGameOrderObj),

        Query = "game_id=7777 AND year=1902 AND season=fall AND phase=order AND sender_country=england",
        {ok, Actual} = game:operator_get_game_msg(Key, Query),

        db:delete (?B_GAME_ORDER, BinID),
        del_game_msg(99991),
        del_game_msg(99992),
        del_game_msg(99993),
        del_game_msg(99994),
        del_game_msg(99995),

        Expected = {[{game_message,undefined,7777,undefined,undefined,england,undefined,undefined,
                undefined,"msg2",2,unread,1902,fall,order},
              {game_message,undefined,7777,undefined,undefined,england,undefined,undefined,
                undefined,"msg1",1,unread,1902,fall,order}],
              [move,support,convoy,hold]},
        ?assertEqual(Expected, Actual)
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
sync_game_msg(GMsg, Countries, Role) ->
    Pid = spawn(?MODULE, receive_init, []),
    register(game_msg, Pid),

    {ok, _GameID} = game:game_msg(GMsg, Countries, Role),
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

sync_delete(ID) ->
    case game:delete_game(ID) of
        ok ->
            ok;
        Other ->
            erlang:error ({error, {{received, Other}, {expected, ok}}})
    end.

put_game_msg(Key, Msg) ->
    BinID = db:int_to_bin(Key),
    % convert record to proplist to be able to do search
    MsgPropList = data_format:rec_to_plist(Msg),
    DbObj = db_obj:create(?B_GAME_MESSAGE, BinID, MsgPropList),
    db:put(DbObj).

del_game_msg(Key) ->
    BinKey = db:int_to_bin(Key),
    db:delete(?B_GAME_MESSAGE, BinKey).
