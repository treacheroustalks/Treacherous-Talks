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

test_key() ->
    "5527647769861816329-1905-spring-order-russia".

test_order_list() ->
    [
     {hold, {army, russia}, prussia},
     {move, {army, germany}, berlin, prussia},
     {move, {fleet, germany}, kiel, berlin},
     {support,
      {army, austria}, budapest,
      {move, {army, austria}, vienna, galicia}},
     {move, {army, austria}, vienna, galicia},
     {move, {army, russia}, warsaw, galicia}
    ].

updated_order_list() ->
    [
     {support,
      {army, austria}, budapest,
      {move, {army, austria}, vienna, galicia}}
    ].

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
      update_game_order_tst_()
     ]}.

ping_tst_ () ->
    [fun()-> {pong, _Pid} = game_worker:ping () end].

%%------------------------------------------------------------------------------
%% Tests the get game state functionality
%%------------------------------------------------------------------------------
get_game_order_tst_ () ->
    [fun() ->
     Key = sync_put_order (test_key(), test_order_list()),
     ?assertEqual(test_key(), Key),
     Move = sync_get_order(test_key()),
     ?assertEqual(test_order_list(), Move)
     end].

update_game_order_tst_ () ->
    [fun() ->
     Key = sync_put_order (test_key(), test_order_list()),
     ?assertEqual(test_key(), Key),
     Updated = sync_update_order (test_key(), updated_order_list()),
     Move = sync_get_order(test_key()),
     ?assertEqual(updated_order_list(), Move)
     end].

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------
sync_put_order (ID, OrderList) ->
    {ok, Key} = game:put_game_order(ID, OrderList),
    Key.

sync_get_order (ID) ->
    case game:get_game_order (ID) of
        {ok, Order} ->
            Order
    end.

sync_update_order (ID, OrderList) ->
    {ok, Key} = game:update_game_order (ID, OrderList),
    Key.