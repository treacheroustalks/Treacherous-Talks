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
-module(order_put_get_test).

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

input_test_order_list() ->
    [
     {hold, army, prussia},
     {move, army, berlin, prussia, nc},
     {move, fleet, kiel, berlin, nc},
     {convoy, army, budapest, army, vienna, galicia},
     {support_move, army, budapest, army, vienna, galicia, nc},
     {support_hold, army, budapest, army, vienna},
     {move, army, vienna, galicia, nc},
     {move, army, warsaw, galicia, nc},
     {build, army, warsaw, nc},
     {disband, army, warsaw}
    ].

input_updated_order_list() ->
    [
     {support_move, army, budapest, army, vienna, galicia, nc}
    ].

expected_test_order_list() ->
    lists:reverse([
     {hold, {army, england}, prussia},
     {move, {army, england}, berlin, prussia},
     {move, {fleet, england}, kiel, berlin},
     {convoy,
      {army, england}, budapest,
      {army, austria}, vienna, galicia},
     {support,
      {army, england}, budapest,
      {move, {army, austria}, vienna, galicia}},
     {support,
      {army, england}, budapest,
      {hold, {army, austria}, vienna}},
     {move, {army, england}, vienna, galicia},
     {move, {army, england}, warsaw, galicia},
     {build, {army, england}, warsaw},
     {disband, {army, england}, warsaw}
    ]).

expected_updated_order_list() ->
    [
     {support,
      {army, england}, budapest,
      {move, {army, austria}, vienna, galicia}}
    ].

expected_full_updated_order_list() ->
    [
     {support,
      {army,england},
      budapest,
      {move,{army,austria},vienna,galicia}},
     {support,
      {army,austria},
      budapest,
      {move,{army,austria},vienna,galicia}}
    ].

expected_key(Id) ->
    game_worker:get_game_order_key(Id, england).

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
      get_game_order_tst_(),
      put_game_order_tst_(),
      get_all_orders_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = game_worker:ping () end].

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

%%------------------------------------------------------------------------------
%% Tests the get game state functionality
%%------------------------------------------------------------------------------

sync_new(Game=#game{}) ->
    {ok, Id} = game:new_game(Game),
    Id.

sync_get(ID) ->
    {ok, Game} = game:get_game(ID),
    Game.

put_game_order_tst_ () ->
    [fun() ->
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             UserID = 1122,
             Country = england,
             game:join_game(Game#game.id, UserID, Country),
             timer:sleep(50),
             % start the game
             game_timer:sync_event(Game#game.id, timeout),
             %%timer:sleep(50),
             {Key,_} = sync_put_order (Game#game.id, UserID, input_test_order_list()),
             ?assertEqual(expected_key(Game#game.id), Key),
             timer:sleep(100),
             sync_put_order (Game#game.id, UserID, input_updated_order_list()),
             timer:sleep(100),
             Move = sync_get_order(Key),
             ?assertEqual(expected_updated_order_list(), Move)
     end].

get_all_orders_tst_ () ->
    [fun() ->
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             %% ENGLAND
             UserID = 1122,
             Country = england,
             game:join_game(Game#game.id, UserID, Country),
             timer:sleep(50),
             %% AUSTRIA
             UserID1 = 1123,
             Country1 = austria,
             game:join_game(Game#game.id, UserID1, Country1),
             timer:sleep(50),

             % start the game
             %game_worker:phase_change(Game, started),
             game_timer:sync_event(Game#game.id, timeout),
             {Key,_} = sync_put_order (Game#game.id, UserID, input_test_order_list()),
             ?assertEqual(expected_key(Game#game.id), Key),
             timer:sleep(100),
             sync_put_order (Game#game.id, UserID, input_updated_order_list()),
             timer:sleep(100),
             sync_put_order (Game#game.id, UserID1, input_updated_order_list()),
             timer:sleep(100),
             Move = sync_get_order(Key),
             ?debugMsg("Get all orders test"),
             ?assertEqual(expected_updated_order_list(), Move),
             FullOrders = game_utils:get_all_orders(Game#game.id),
             ExpectedFullOrders = expected_full_updated_order_list(),
             ?assertEqual(length(FullOrders), length(ExpectedFullOrders)),
             lists:foreach(fun(Order) ->
                                ?assert(lists:member(Order, ExpectedFullOrders))
                           end, FullOrders)
     end].

get_game_order_tst_ () ->
    [fun() ->
             GameRecord = test_game(),
             % Create a new Game
             Game = sync_get(sync_new(GameRecord)),
             % join new player with id=1122 and country=england
             UserID = 1122,
             Country = england,
             game:join_game(Game#game.id, UserID, Country),
             timer:sleep(100),
             % start the game
             %game_worker:phase_change(Game#game{status=ongoing}, started),
             %timer:sleep(50),
             game_timer:sync_event(Game#game.id, timeout),
             {Key,_} = sync_put_order (Game#game.id, UserID, input_test_order_list()),
             ?assertEqual(expected_key(Game#game.id), Key),
             timer:sleep(100),
             Move = sync_get_order(Key),
             ?assertEqual(expected_test_order_list(), Move)
     end].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
sync_put_order (GameId, UserId, OrderList) ->
    {ok, Key} = game:put_game_order(GameId, UserId, OrderList),
    Key.

sync_get_order (Key) ->
    case game_utils:get_game_order (Key) of
        {ok, Order} ->
            Order;
        Error ->
            Error
    end.
