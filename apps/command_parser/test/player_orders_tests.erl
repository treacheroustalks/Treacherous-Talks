%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(player_orders_tests).

-import(player_orders, [interpret_str_orders/1,init_valid_region/0,
        translate_abbv_to_fullname_atom/1,translate_location/1,interpret_order_line/1]).
-include_lib("eunit/include/eunit.hrl").

-include("test_utils.hrl").
-include("player_orders.hrl").

parse_orders_test_() ->
    {match,[Out]}=re:run(?SAMPLE_TEST_ORDERS1, "ORDER(.*)END",
                         [{capture, all_but_first, list}, dotall]),
    ActualOutput = player_orders:parse_orders(Out),
   % io:format(user, "~p~n", [ActualOutput]),
    Expected = {ok,{session,1234341},
    {gameid,3958230945903},
    {[{move,fleet,mid_atlantic_ocean,north_atlantic_ocean,north_coast},
      {waive},
      {build,fleet,munich,north_coast},
      {remove,army,munich},
      {disband,army,munich},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_hold,fleet,finland,any_unit,brest},
      {support_hold,fleet,finland,any_unit,brest},
      {hold,any_unit,holland},
      {hold,army,brest},
      {hold,army,brest},
      {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,finland,army,brest,marseilles},
      {move,army,brest,marseilles,any_coast},
      {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,finland,army,brest,marseilles},
      {move,army,brest,marseilles,any_coast},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {move,army,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,north_coast},
      {move,any_unit,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,any_coast}],
     [{error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",["A","Bre","->","F",[],[],[]]}},
      {error,{"invalid action#",["A","Bre","-","F",[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}}]}},
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
    Expected = adriatic_sea,
    ActualOutput2 = translate_location("adriatic_sea"),
    Expected2 = adriatic_sea,
    [
        ?_assertEqual(Expected, ActualOutput),
        ?_assertEqual(Expected2, ActualOutput2)
    ].

interpret_str_orders_test_() ->
    ActualOutput = interpret_str_orders(["a boh s a vie-nrg",
                                         "fleet nrg hold"]),
    Expected = [{hold,fleet,norwegian_sea},
                {support_move,army,bohemia,army,vienna,norwegian_sea,any_coast}],
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

interpret_order_line_test_() ->
    ActualOutput = interpret_order_line(["a","boh","s","a","vie","nrg","nc"]),
    Expected = {support_move,army,bohemia,army,vienna,norwegian_sea,north_coast},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

translate_abbv_to_fullname_atom_test_() ->
    Fullnames = lists:map(fun(X) -> {N,_} = X, N end, ?TRANS_LOC_ABBV),
    ActualOutput = [translate_abbv_to_fullname_atom(Y)|| Y <- Fullnames],
    Expected = ?LOCATIONS,
    [
        ?_assertEqual(Expected, ActualOutput)
    ].