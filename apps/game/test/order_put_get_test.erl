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
     {support_move, army, budapest, army, vienna, galicia, nc},
     {move, army, vienna, galicia, nc},
     {move, army, warsaw, galicia, nc}
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
     {support,
      {army, england}, budapest,
      {move, {army, austria}, vienna, galicia}},
     {move, {army, england}, vienna, galicia},
     {move, {army, england}, warsaw, galicia}
    ]).

expected_updated_order_list() ->
    [
     {support,
      {army, england}, budapest,
      {move, {army, austria}, vienna, galicia}}
    ].

expected_key(Id) ->
    integer_to_list(Id) ++ "-1900-fall-order-england".

%%------------------------------------------------------------------------------
%% @doc
%%  the top level test
%% @end
%%------------------------------------------------------------------------------
move_get_put_test_ () ->
    {setup,
     fun app_started_setup/0,
     fun app_started_teardown/1,
     [ping_tst_ (),
      get_game_order_tst_ (),
      put_game_order_tst_()
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
             Key = sync_put_order (Game#game.id, UserID, input_test_order_list()),
             ?assertEqual(expected_key(Game#game.id), Key),
             sync_put_order (Game#game.id, UserID, input_updated_order_list()),
             Move = sync_get_order(Key),
             ?assertEqual(expected_updated_order_list(), Move)
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
             timer:sleep(50),
             Key = sync_put_order (Game#game.id, UserID, input_test_order_list()),
             ?assertEqual(expected_key(Game#game.id), Key),
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
    case game_worker:get_game_order (Key) of
        {ok, Order} ->
            Order;
        Error ->
            Error
    end.
