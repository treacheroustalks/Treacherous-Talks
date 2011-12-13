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
    io:format(user, "~p~n", [ActualOutput]),
    Expected = {ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==",
    {3958230945903,
     [{build,fleet,munich,north_coast},
      {build,fleet,munich,north_coast},
      {disband,army,munich},
      {disband,army,munich},
      {support_move,army,munich,army,belgium,berlin,any_coast},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_hold,fleet,finland,army,brest},
      {support_hold,fleet,marseilles,army,albania},
      {support_hold,fleet,finland,army,brest},
      {hold,army,holland},
      {hold,army,brest},
      {hold,army,munich},
      {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,finland,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,north_sea,army,yorkshire,norway},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {move,army,brest,marseilles,any_coast},
      {move,fleet,mid_atlantic_ocean,north_atlantic_ocean,north_coast},
      {move,army,brest,marseilles,any_coast},
      {move,army,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,north_coast},
      {move,army,belgium,munich,any_coast},
      {move,army,london,norwegian_sea,any_coast}]}},
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