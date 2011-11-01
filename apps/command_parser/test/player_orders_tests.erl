%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(player_orders_tests).

-import(player_orders, [interpret_str_orders/1,init_valid_region/0,
        translate_fullname_to_abbv_atom/1,translate_location/1,interpret_order_line/1]).
-include_lib("eunit/include/eunit.hrl").

-include("test_utils.hrl").
-include("player_orders.hrl").

parse_orders_test_() ->
    ActualOutput = player_orders:parse_orders(binary_to_list(?SAMPLE_TEST_ORDERS1)),
    io:format(user, "~p~n", [ActualOutput]),
    Expected = {ok,{[{move,fleet,mid,nat,north_coast},
      {waive},
      {build,fleet,mun,north_coast},
      {remove,army,mun},
      {disband,army,mun},
      {support_move,army,mun,fleet,kie,ber,any_coast},
      {support_move,army,mun,fleet,kie,ber,any_coast},
      {support_hold,fleet,fin,any_unit,bre},
      {support_hold,fleet,fin,any_unit,bre},
      {hold,any_unit,hol},
      {hold,army,bre},
      {hold,army,bre},
      {convoy,fleet,gol,army,bre,mar},
      {convoy,fleet,wes,army,bre,mar},
      {convoy,fleet,fin,army,bre,mar},
      {move,army,bre,mar,any_coast},
      {convoy,fleet,gol,army,bre,mar},
      {convoy,fleet,wes,army,bre,mar},
      {convoy,fleet,fin,army,bre,mar},
      {move,army,bre,mar,any_coast},
      {convoy,fleet,nth,army,lon,nrg},
      {convoy,fleet,nth,army,lon,nrg},
      {move,army,lon,nrg,any_coast},
      {move,army,lon,nrg,north_coast},
      {move,any_unit,lon,nrg,any_coast},
      {move,army,lon,nrg,any_coast}],
     [{error,"dfadfasdfaldfa#invalid full name"},
      {error,{"invalid action#",[[],[],"d",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",["a","bre","->","f",[],[],[]]}},
      {error,{"invalid action#",["a","bre","-","f",[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,"eid#invalid location name, not in atom table"},
      {error,"ses#invalid location name, not in atom table"},
      {error,"orde#invalid full name"},
      {error,"ddfaadfaff#invalid full name"}]}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

init_valid_region_test_() ->
    init_valid_region(),
    ActualOutput = lists:all(fun(X)-> get(X) end, ?LOCATIONS),
    Expected = true,
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

translate_location_test_() ->
    ActualOutput = translate_location("adr"),
    Expected = adr,
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

interpret_str_orders_test_() ->
    ActualOutput = interpret_str_orders(["a boh s a vie-nrg",
                                         "fleet nrg hold"]),
    Expected = [{hold,fleet,nrg},
                {support_move,army,boh,army,vie,nrg,any_coast}],
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

interpret_order_line_test_() ->
    ActualOutput = interpret_order_line(["a","boh","s","a","vie","nrg","nc"]),
    Expected = {support_move,army,boh,army,vie,nrg,north_coast},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

translate_fullname_to_abbv_atom_test_() ->
    Fullnames = lists:map(fun(X) -> {N,_} = X, N end, ?TRANS_LOC_FULLNAME),
    ActualOutput = [translate_fullname_to_abbv_atom(Y)|| Y <- Fullnames],
    Expected = ?LOCATIONS,
    [
        ?_assertEqual(Expected, ActualOutput)
    ].