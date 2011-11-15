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
-module (rules_test).

-include_lib ("eunit/include/eunit.hrl").

units_exist_test () ->
    Map = map_data:create (standard_game),
    %% there is no army in galicia!
    rules:process (order_phase, Map, diplomacy_rules, [{move, {army, austria},
                                              galicia, ukraine}]),
    ?assertEqual ([], map:get_units (Map, galicia)),
    ?assertEqual ([], map:get_units (Map, ukraine)),
    map_data:delete (Map).

unit_cannot_go_there_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, austria}, budapest, tyrolia}]),
    ?assertEqual ([], map:get_units (Map, tyrolia)),
    ?assertEqual ([{army, austria}], map:get_units (Map, budapest)),
    map_data:delete (Map).

bounce_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, austria}, vienna, galicia},
                    {move, {army, russia}, warsaw, galicia}]),
    ?assertEqual ([], map:get_units (Map, galicia)),
    map_data:delete (Map).

hold_vs_move_test () ->
    Map = map_data:create (standard_game),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {fleet, austria}, trieste, venice},
                    {hold, {army, italy}, venice}]),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    map_data:delete (Map).

support_added_test () ->
    Map = map_data:create (standard_game),
    ?assert (map:unit_exists (Map, budapest, {army, austria})),
    ?assert (map:unit_exists (Map, vienna, {army, austria})),
    ?assert (map:unit_exists (Map, munich, {army, germany})),
    ?assert (map:unit_exists (Map, venice, {army, italy})),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{support,
                     {army, austria}, budapest,
                     {hold, {army, austria}, vienna}},
                    {hold, {army, austria}, vienna},
                    {hold, {army, italy}, venice},
                   {support,
                    {army, germany}, munich,
                    {hold, {army, italy}, venice}}]),
    ?assertEqual ([{support,
                     {army, austria}, budapest,
                     {hold, {army, austria}, vienna}}],
                  map:get_unit_info (Map, {army, austria}, vienna,
                                     support_orders, no_support)),
    ?assertEqual (no_support,
                  map:get_unit_info (Map, {army, italy}, venice,
                                     support_orders, no_support)),
    map_data:delete (Map).

% move A vie gal
% support A bud A vie gal
% move A war gal
support_stronger_unit_moves_test () ->
    Map = map_data:create (standard_game),
    ?assert (map:unit_exists (Map, budapest, {army, austria})),
    ?assert (map:unit_exists (Map, vienna, {army, austria})),
    ?assert (map:unit_exists (Map, warsaw, {army, russia})),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{support,
                     {army, austria}, budapest,
                     {move, {army, austria}, vienna, galicia}},
                    {move, {army, austria}, vienna, galicia},
                    {move, {army, russia}, warsaw, galicia}]),
    ?assertEqual ([{army, austria}], map:get_units (Map, galicia)),
    ?assertEqual ([], map:get_units (Map,vienna)),
    ?assertEqual ([{army, austria}], map:get_units (Map, budapest)),
    ?assertEqual ([{army, russia}], map:get_units (Map, warsaw)),
    ?assertEqual (undefined,
                  map:get_unit_info (Map, {army, italy}, venice,
                                     strength)),
    map_data:delete (Map).

%% manual diagram 5
diagram_5_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, russia}, warsaw, prussia}]),
    ?assert (map:unit_exists (Map, kiel, {fleet, germany})),
    ?assert (map:unit_exists (Map, berlin, {army, germany})),
    ?assert (map:unit_exists (Map, prussia, {army, russia})),

    rules:process (order_phase, Map, diplomacy_rules,
                   [{hold, {army, russia}, prussia},
                    {move, {army, germany}, berlin, prussia},
                    {move, {fleet, germany}, kiel, berlin}]),
    ?assertEqual ([{army, russia}], map:get_units (Map, prussia)),
    ?assertEqual ([{army, germany}], map:get_units (Map,berlin)),
    ?assertEqual ([{fleet, germany}], map:get_units (Map, kiel)),
    map_data:delete (Map).

%% manual diagram 6 - in a different location, for convenience
diagram_6_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {fleet, austria}, trieste, venice},
                        {move, {army, italy}, venice, trieste}]),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    map_data:delete (Map).

%% manual diagram 7
diagram_7_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {fleet, germany}, kiel, holland},
                    {move, {army, germany}, berlin, kiel},
                    {move, {fleet, england}, london, north_sea}]),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {fleet, germany}, holland, belgium},
                    {move, {army, germany}, kiel, holland}]),
    ?assert (map:unit_exists (Map, north_sea, {fleet, england})),
    ?assert (map:unit_exists (Map, belgium, {fleet, germany})),
    ?assert (map:unit_exists (Map, holland, {army, germany})),

    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {fleet, england}, north_sea, holland},
                    {move, {army, germany}, holland, belgium},
                    {move, {fleet, germany}, belgium, north_sea}]),
    ?assert (map:unit_exists (Map, holland, {fleet, england})),
    ?assert (map:unit_exists (Map, north_sea, {fleet, germany})),
    ?assert (map:unit_exists (Map, belgium, {army, germany})),
    map_data:delete (Map).

diagram_8_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, france}, paris, gascony},
                    {move, {army, germany}, munich, burgundy}]),
    ?assert (map:unit_exists (Map, gascony, {army, france})),
    ?assert (map:unit_exists (Map, marseilles, {army, france})),
    ?assert (map:unit_exists (Map, burgundy, {army, germany})),

    Response = rules:process (order_phase, Map, diplomacy_rules,
                              [{support, {army, france}, gascony,
                                {move, {army, france}, marseilles, burgundy}},
                               {move, {army, france}, marseilles, burgundy},
                               {hold, {army, germany}, burgundy}]),
    ?assert (lists:member ({dislodge, {army, germany}, burgundy}, Response)),
    ?assert (map:unit_exists (Map, burgundy, {army, france})),
    ?assert (map:unit_exists (Map, burgundy, {army, germany})),
    ?assert (map:unit_exists (Map, gascony, {army, france})),
    map_data:delete (Map).

diagram_9_test () ->
    Map = map_data:create (standard_game),
    map:add_unit (Map, {fleet, germany}, baltic_sea),
    map:add_unit (Map, {army, russia}, prussia),
    map:add_unit (Map, {army, germany}, silesia),
    ?assert (map:unit_exists (Map, baltic_sea, {fleet, germany})),
    ?assert (map:unit_exists (Map, silesia, {army, germany})),
    ?assert (map:unit_exists (Map, prussia, {army, russia})),
    Response =
        rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {army, germany}, silesia, prussia},
                        {support, {fleet, germany}, baltic_sea,
                         {move, {army, germany}, silesia, prussia}},
                        {hold, {army, russia}, prussia}]),
    ?assert (lists:member ({dislodge, {army, russia}, prussia}, Response)),
    ?assert (map:unit_exists (Map, prussia, {army, germany})),
    map_data:delete (Map).

diagram_10_test () ->
    Map = map_data:create (standard_game),
    map:add_unit (Map, {fleet, france}, western_mediterranean),
    map:add_unit (Map, {fleet, france}, gulf_of_lyon),
    map:remove_unit (Map, {army, italy}, rome),
    map:add_unit (Map, {fleet, italy}, rome),
    ?assert (map:unit_exists (Map, gulf_of_lyon, {fleet, france})),
    ?assert (map:unit_exists (Map, western_mediterranean, {fleet, france})),
    ?assert (map:unit_exists (Map, rome, {fleet, italy})),
    ?assert (map:unit_exists (Map, naples, {fleet, italy})),
    _Response =
        rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {fleet, italy}, naples, tyrrhenian_sea},
                        {support, {fleet, italy}, rome,
                         {move, {fleet, italy}, naples, tyrrhenian_sea}},
                        {move, {fleet, france}, gulf_of_lyon, tyrrhenian_sea},
                        {support, {fleet, france}, western_mediterranean,
                         {move, {fleet, france}, gulf_of_lyon,
                          tyrrhenian_sea}}]),
    ?assert (map:unit_exists (Map, gulf_of_lyon, {fleet, france})),
    ?assert (map:unit_exists (Map, western_mediterranean, {fleet, france})),
    ?assert (map:unit_exists (Map, rome, {fleet, italy})),
    ?assert (map:unit_exists (Map, naples, {fleet, italy})),
    map_data:delete (Map).

diagram_11_test () ->
    Map = map_data:create (standard_game),
    map:add_unit (Map, {fleet, france}, western_mediterranean),
    map:add_unit (Map, {fleet, france}, gulf_of_lyon),
    map:remove_unit (Map, {fleet, italy}, naples),
    map:remove_unit (Map, {army, italy}, rome),
    map:add_unit (Map, {fleet, italy}, rome),
    map:add_unit (Map, {fleet, italy}, tyrrhenian_sea),
    ?assert (map:unit_exists (Map, gulf_of_lyon, {fleet, france})),
    ?assert (map:unit_exists (Map, western_mediterranean, {fleet, france})),
    ?assert (map:unit_exists (Map, rome, {fleet, italy})),
    ?assert (map:unit_exists (Map, tyrrhenian_sea, {fleet, italy})),
    _Response =
        rules:process (order_phase, Map, diplomacy_rules,
                       [{hold, {fleet, italy}, tyrrhenian_sea},
                        {support, {fleet, italy}, rome,
                         {hold, {fleet, italy}, tyrrhenian_sea}},
                        {move, {fleet, france}, gulf_of_lyon, tyrrhenian_sea},
                        {support, {fleet, france}, western_mediterranean,
                         {move, {fleet, france}, gulf_of_lyon,
                          tyrrhenian_sea}}]),
    ?assert (map:unit_exists (Map, gulf_of_lyon, {fleet, france})),
    ?assert (map:unit_exists (Map, western_mediterranean, {fleet, france})),
    ?assert (map:unit_exists (Map, rome, {fleet, italy})),
    ?assert (map:unit_exists (Map, tyrrhenian_sea, {fleet, italy})),
    map_data:delete (Map).

diagram_12_test () ->
    Map = map_data:create (standard_game),
    map:add_unit (Map, {army, russia}, prussia),
    map:add_unit (Map, {army, austria}, bohemia),
    map:add_unit (Map, {army, austria}, tyrolia),
    ?assert (map:unit_exists (Map, munich, {army, germany})),
    ?assert (map:unit_exists (Map, berlin, {army, germany})),
    ?assert (map:unit_exists (Map, prussia, {army, russia})),
    ?assert (map:unit_exists (Map, warsaw, {army, russia})),
    ?assert (map:unit_exists (Map, bohemia, {army, austria})),
    ?assert (map:unit_exists (Map, tyrolia, {army, austria})),
    _Response =
        rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {army, austria}, bohemia, munich},
                        {support, {army, austria}, tyrolia,
                         {move, {army, austria}, bohemia, munich}},
                        {move, {army, germany}, munich, silesia},
                        {support, {army, germany}, berlin,
                         {move, {army, germany}, munich, silesia}},
                        {move, {army, russia}, warsaw, silesia},
                        {support, {army, russia}, prussia,
                         {move, {army, russia}, warsaw, silesia}}]),
    ?assert (map:unit_exists (Map, munich, {army, germany})),
    ?assert (map:unit_exists (Map, munich, {army, austria})),
    ?assert (map:unit_exists (Map, prussia, {army, russia})),
    ?assert (map:unit_exists (Map, warsaw, {army, russia})),
    ?assert (map:unit_exists (Map, tyrolia, {army, austria})),
    map_data:delete (Map).

multiple_stages_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, france}, paris, gascony},
                    {move, {army, germany}, munich, burgundy}]),
    ?assert (map:unit_exists (Map, gascony, {army, france})),
    ?assert (map:unit_exists (Map, marseilles, {army, france})),
    ?assert (map:unit_exists (Map, burgundy, {army, germany})),

    Response = rules:process (order_phase, Map, diplomacy_rules,
                              [{support, {army, france}, gascony,
                                {move, {army, france}, marseilles, burgundy}},
                               {move, {army, france}, marseilles, burgundy},
                               {hold, {army, germany}, burgundy}]),
    ?assert (lists:member ({dislodge, {army, germany}, burgundy}, Response)),
    ?assert (map:unit_exists (Map, burgundy, {army, france})),
    ?assert (map:unit_exists (Map, burgundy, {army, germany})),
    ?assert (map:unit_exists (Map, gascony, {army, france})),
    %% germany fails to retreat A burgundy - it is destroyed:
    rules:process (retreat_phase, Map, diplomacy_rules, []),
    ?assertEqual (false, map:unit_exists (Map, burgundy, {army, germany})),
    map_data:delete (Map).

build_reply_test () ->
%    ?debugMsg ("###################################### BUILD-REPLY: START"),
    Map = map_data:create (standard_game),
    map:add_unit (Map, {army, austria}, albania),
    map:remove_unit (Map, {fleet, england}, london),
    Reply = rules:process (count_phase, Map, diplomacy_rules, []),
    ?assert (lists:member ({has_builds, austria, -1}, Reply)),
    ?assert (lists:member ({has_builds, england, 1}, Reply)),
%    ?debugMsg ("###################################### BUILD-REPLY: DONE"),
    map_data:delete (Map).


-define (SUCCESSFUL_MOVE (U, Fr, To), {{move, U, Fr, To}, {U, To}}).
-define (UNSUCCESSFUL_MOVE (U, Fr, To), {{move, U, Fr, To}, {U, Fr}}).
-define (SUCCESSFUL_HOLD (U, Wh), {{hold, U, Wh}, {U, Wh}}).
-define (SUCCESSFUL_CONVOY (Fleet, Wh, Army, Fr, To),
         {{convoy, Fleet, Wh, Army, Fr, To}, {Army, To}}).
-define (SUPPORT (U, Wh, Or), {{support, U, Wh, Or}, {U, Wh}}).
-define (BUILD (U, Wh), {{build, U, Wh}, {U, Wh}}).

-spec tmoat_data (Season, Phase, Year) -> [{Order, {Unit, IsWhere}}] when
      Season :: atom (),
      Phase :: atom (),
      Year :: pos_integer (),
      Order :: tuple (),
      Unit :: tuple (),
      IsWhere :: atom ().
tmoat_data (spring, order_phase, 1901) ->
    [?SUCCESSFUL_MOVE ({army, austria}, vienna, trieste),
     ?UNSUCCESSFUL_MOVE ({army, austria}, budapest, galicia),
     ?SUCCESSFUL_MOVE ({fleet, austria}, trieste, albania),
     ?SUCCESSFUL_MOVE ({army, england}, liverpool, yorkshire),
     ?SUCCESSFUL_MOVE ({fleet, england}, london, north_sea),
     ?SUCCESSFUL_MOVE ({fleet, england}, edinburgh, norwegian_sea),
     ?SUCCESSFUL_MOVE ({army, france}, paris, burgundy),
     ?SUCCESSFUL_MOVE ({army, france}, marseilles, spain),
     ?SUCCESSFUL_MOVE ({fleet, france}, brest, picardy),
     ?SUCCESSFUL_MOVE ({army, germany}, berlin, kiel),
     ?SUCCESSFUL_MOVE ({army, germany}, munich, ruhr),
     ?SUCCESSFUL_MOVE ({fleet, germany}, kiel, denmark),
     ?SUCCESSFUL_MOVE ({army, italy}, venice, piedmont),
     ?SUCCESSFUL_MOVE ({army, italy}, rome, venice),
     ?SUCCESSFUL_MOVE ({fleet, italy}, naples, ionian_sea),
     ?SUCCESSFUL_MOVE ({army, russia}, moscow, ukraine),
     ?UNSUCCESSFUL_MOVE ({army, russia}, warsaw, galicia),
     ?SUCCESSFUL_MOVE ({fleet, russia}, st_petersburg, gulf_of_bothnia),
     ?UNSUCCESSFUL_MOVE ({fleet, russia}, sevastopol, black_sea),
     ?SUCCESSFUL_MOVE ({army, turkey}, constantinople, bulgaria),
     ?SUCCESSFUL_MOVE ({army, turkey}, smyrna, constantinople),
     ?UNSUCCESSFUL_MOVE ({fleet, turkey}, ankara, black_sea)
];
tmoat_data (fall, order_phase, 1901) ->
    [?SUCCESSFUL_HOLD ({army, austria}, trieste),
     ?UNSUCCESSFUL_MOVE ({army, austria}, budapest, serbia),
     ?SUCCESSFUL_MOVE ({fleet, austria}, albania, greece),
     ?SUCCESSFUL_MOVE ({army, england}, yorkshire, norway),
     ?SUCCESSFUL_CONVOY ({fleet, england}, north_sea,
                         {army, england}, yorkshire, norway),
     ?SUCCESSFUL_MOVE ({fleet, england}, norwegian_sea, barents_sea),
     ?UNSUCCESSFUL_MOVE ({army, france}, burgundy, marseilles),
     ?SUCCESSFUL_MOVE ({army, france}, spain, portugal),
     ?UNSUCCESSFUL_MOVE ({fleet, france}, picardy, belgium),
     ?SUCCESSFUL_MOVE ({army, germany}, kiel, holland),
     ?UNSUCCESSFUL_MOVE ({army, germany}, ruhr, belgium),
     ?SUCCESSFUL_HOLD ({fleet, germany}, denmark),
     ?SUCCESSFUL_HOLD ({army, italy}, venice),
     ?UNSUCCESSFUL_MOVE ({army, italy}, piedmont, marseilles),
     ?SUCCESSFUL_MOVE ({fleet, italy}, ionian_sea, tunis),
     ?SUPPORT ({army, russia}, ukraine,
               {move, {army, russia}, sevastopol, rumania}),
     ?SUCCESSFUL_MOVE ({army, russia}, warsaw, galicia),
     ?SUCCESSFUL_MOVE ({fleet, russia}, sevastopol, rumania),
     ?SUCCESSFUL_MOVE ({fleet, russia}, gulf_of_bothnia, sweden),
     ?UNSUCCESSFUL_MOVE ({army, turkey}, bulgaria, serbia),
     ?UNSUCCESSFUL_MOVE ({army, turkey}, constantinople, bulgaria),
     ?SUCCESSFUL_MOVE ({fleet, turkey}, ankara, black_sea)];
tmoat_data (fall, build_phase, 1901) ->
    [?BUILD ({fleet, england}, edinburgh),
     ?BUILD ({fleet, germany}, kiel),
     ?BUILD ({army, germany}, munich),
     ?BUILD ({army, russia}, st_petersburg),
     ?BUILD ({army, russia}, sevastopol),
     ?BUILD ({army, turkey}, smyrna),
     ?BUILD ({army, austria}, vienna),
     ?BUILD ({fleet, italy}, naples),
     ?BUILD ({fleet, france}, marseilles)];
tmoat_data (spring, order_phase, 1902) ->
    [?UNSUCCESSFUL_MOVE ({army, austria}, trieste, budapest),
     ?UNSUCCESSFUL_MOVE ({army, austria}, vienna, budapest),
     ?SUCCESSFUL_MOVE ({army, austria}, budapest, serbia),
     ?SUCCESSFUL_HOLD ({fleet, austria}, greece),
     ?UNSUCCESSFUL_MOVE ({army, england}, norway, st_petersburg),
     ?UNSUCCESSFUL_MOVE ({fleet, england}, north_sea, norway),
     ?SUPPORT ({fleet, england}, barents_sea,
               {move, {army, england}, norway, st_petersburg}),
     ?SUPPORT ({army, france}, burgundy,
               {move, {fleet, france}, picardy, belgium}),
     ?SUCCESSFUL_MOVE ({army, france}, portugal, spain),
     ?UNSUCCESSFUL_MOVE ({fleet, france}, picardy, belgium),
     ?SUCCESSFUL_HOLD ({fleet, france}, marseilles),
     ?SUCCESSFUL_MOVE ({army, germany}, holland, belgium),
     ?SUPPORT ({army, germany}, ruhr,
               {move, {army, germany}, holland, belgium}),
     ?UNSUCCESSFUL_MOVE ({army, germany}, munich, burgundy),
     ?SUCCESSFUL_HOLD ({fleet, germany}, denmark),
     ?SUCCESSFUL_MOVE ({fleet, germany}, kiel, holland),
     ?SUCCESSFUL_HOLD ({army, italy}, venice),
     ?UNSUCCESSFUL_MOVE ({army, italy}, piedmont, marseilles),
     ?SUCCESSFUL_MOVE ({fleet, italy}, tunis, western_mediterranean),
     ?SUCCESSFUL_MOVE ({fleet, italy}, naples, tyrrhenian_sea),
     ?SUPPORT ({army, russia}, ukraine,
               {hold, {fleet, russia}, rumania}),
     ?UNSUCCESSFUL_MOVE ({army, russia}, galicia, budapest),
     ?UNSUCCESSFUL_MOVE ({army, russia}, st_petersburg, norway),
     ?SUPPORT ({army, russia}, sevastopol,
               {hold, {fleet, russia}, rumania}),
     ?SUPPORT ({fleet, russia}, sweden,
               {move, {army, russia}, st_petersburg, norway}),
     ?SUCCESSFUL_HOLD ({fleet, russia}, rumania),
     ?UNSUCCESSFUL_MOVE ({army, turkey}, bulgaria, rumania),
     ?UNSUCCESSFUL_MOVE ({army, turkey}, constantinople, bulgaria),
     ?SUCCESSFUL_MOVE ({army, turkey}, smyrna, armenia),
     ?SUPPORT ({fleet, turkey}, black_sea,
               {move, {army, turkey}, bulgaria, rumania})];
tmoat_data (fall, order_phase, 1902) ->
    [?UNSUCCESSFUL_MOVE ({army, austria}, vienna, galicia),
     ?SUCCESSFUL_MOVE ({army, austria}, trieste, budapest),
     ?SUPPORT ({army, austria}, serbia,
               {move, {army, turkey}, bulgaria, rumania}),
     ?SUCCESSFUL_HOLD ({fleet, austria}, greece),
     ?SUCCESSFUL_MOVE ({army, england}, norway, st_petersburg),
     ?SUPPORT ({fleet, england}, barents_sea,
               {move, {army, england}, norway, st_petersburg}),
     ?SUCCESSFUL_MOVE ({fleet, england}, north_sea, norway),
     ?SUCCESSFUL_MOVE ({fleet, england}, edinburgh, north_sea),
     ?SUCCESSFUL_MOVE ({army, england}, norway, st_petersburg),
     ?UNSUCCESSFUL_MOVE ({army, france}, burgundy, belgium),
     ?SUPPORT ({fleet, france}, picardy,
               {move, {army, france}, burgundy, belgium}),
     ?SUPPORT ({army, france}, spain,
               {hold, {fleet, france}, marseilles}),
     ?SUPPORT ({fleet, france}, marseilles,
               {hold, {army, france}, spain}), %% unsuccessful support due to IT
     ?SUCCESSFUL_MOVE ({army, germany}, ruhr, burgundy),
     ?SUPPORT ({army, germany}, munich,
               {move, {army, germany}, ruhr, burgundy}),
     ?SUPPORT ({army, germany}, belgium,
               {move, {army, germany}, ruhr, burgundy}),
     ?UNSUCCESSFUL_MOVE ({fleet, germany}, denmark, sweden),
     ?SUPPORT ({fleet, germany}, holland,
               {hold, {army, germany}, belgium}),
     ?UNSUCCESSFUL_MOVE ({army, italy}, venice, piedmont),
     ?UNSUCCESSFUL_MOVE ({army, italy}, piedmont, marseilles),
     ?SUCCESSFUL_MOVE ({fleet, italy}, western_mediterranean, north_africa),
     ?SUCCESSFUL_MOVE ({fleet, italy}, tyrrhenian_sea, gulf_of_lyon),
     ?UNSUCCESSFUL_MOVE ({army, russia}, st_petersburg, norway),
     ?SUPPORT ({fleet, russia}, sweden,
               {move, {army, russia}, st_petersburg, norway}),
     ?SUPPORT ({fleet, russia}, rumania,
               {hold, {army, russia}, sevastopol}),
     ?SUCCESSFUL_HOLD ({army, russia}, sevastopol),
     ?SUPPORT ({army, russia}, sevastopol,
               {hold, {fleet, russia}, rumania}),
     ?SUPPORT ({army, russia}, galicia,
               {hold, {fleet, russia}, rumania}),
     ?SUPPORT ({army, russia}, ukraine,
               {hold, {army, russia}, sevastopol}),
     ?SUCCESSFUL_MOVE ({army, turkey}, bulgaria, rumania),
     ?SUCCESSFUL_MOVE ({army, turkey}, constantinople, bulgaria),
     ?UNSUCCESSFUL_MOVE ({army, turkey}, armenia, sevastopol),
     ?SUPPORT ({fleet, turkey}, black_sea,
               {move, {army, turkey}, bulgaria, rumania})];
tmoat_data (_, _, _) ->
    [].

%% The game that is sketched on the last pages of the manual (starting on
%% page 20)
the_mother_of_all_tests_test () ->
    Map = map_data:create (standard_game),
    lists:foreach (fun (Year) -> tmoat_year (Map, Year) end,
                   [1901,
                    1902]),
    map_data:delete (Map).

check_results (_Description, Map, Results) ->
    io:format (user, "asserting existence: ", []),
    lists:foreach (fun ({Unit, ShouldBe}) ->
                           io:format (user, "of ~p.. | ",
                                      [{Unit, ShouldBe}]),
                           ?assert (map:unit_exists (Map, ShouldBe, Unit))
                   end,
                   Results),
    io:format (user, "~n", []).

tmoat_year (Map, Year) ->
    ?debugMsg (io_lib:format ("spring ~p orders", [Year])),
    {SpringOrders, SpringResults} =
        lists:unzip (
          tmoat_data (spring, order_phase, Year)),
    rules:process (order_phase, Map, diplomacy_rules, SpringOrders),
    check_results ({spring, Year}, Map, SpringResults),

    ?debugMsg (io_lib:format ("spring ~p retreat", [Year])),
    {SpringRetreatOrders, SpringRetreatResults} =
        lists:unzip (
          tmoat_data (spring, retreat_phase, Year)),
    rules:process (retreat_phase, Map, diplomacy_rules,
                   SpringRetreatOrders),
    check_results ({spring_retreat, Year}, Map, SpringRetreatResults),

    ?debugMsg (io_lib:format ("fall ~p orders", [Year])),
    {FallOrders, FallResults} =
        lists:unzip (tmoat_data (fall, order_phase, Year)),
    rules:process (order_phase, Map, diplomacy_rules, FallOrders),
    check_results ({fall_retreat, Year}, Map, FallResults),

    ?debugMsg (io_lib:format ("fall ~p retreat", [Year])),
    {FallRetreatOrders, FallRetreatResults} =
        lists:unzip (
          tmoat_data (fall, retreat_phase, Year)),
    rules:process (retreat_phase, Map, diplomacy_rules, FallRetreatOrders),
    check_results ({fall_retreat, Year}, Map, FallRetreatResults),

    ?debugMsg (io_lib:format ("fall ~p count", [Year])),
    rules:process (count_phase, Map, diplomacy_rules, []),

    ?debugMsg (io_lib:format ("fall ~p build", [Year])),
    {BuildOrders, BuildResults} =
        lists:unzip (
          tmoat_data (fall, build_phase, Year)),
    rules:process (build_phase, Map, diplomacy_rules, BuildOrders),
    check_results ({fall_build, Year}, Map, BuildResults).

implicit_hold_test () ->
    Map = map_data:create (empty),
    map:add_province (Map, vienna),
    map:add_unit (Map, {army, austria}, vienna),
    Reply = rules:process (order_phase, Map, diplomacy_rules, []),
    ?assertEqual ([{added, {hold, {army, austria}, vienna}}], Reply),
    map_data:delete (Map).

%% check that no build orders are accepted that go above what the nation is
%% allowed to do
limit_builds_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                   [{move, {army, austria}, vienna, galicia}]),
    ?assertEqual ([], map:get_units (Map, vienna)),
    ?debugVal (rules:process (count_phase, Map, diplomacy_rules,[])),
    Reply = rules:process (build_phase, Map, diplomacy_rules,
                   [{build, {army, austria}, vienna}]),
    ?assertEqual ([], map:get_units (Map, vienna)),
    ?assertEqual ([{no_builds_left, {build, {army, austria}, vienna}}], Reply),
    map:remove_unit (Map, {army, austria}, galicia),
    Reply2 = rules:process (build_phase, Map, diplomacy_rules,
                   [{build, {army, austria}, vienna}]),
    ?assertEqual ([], Reply2),
    ?assert (map:unit_exists (Map, vienna, {army, austria})),
    map_data:delete (Map).

civil_disorder_test () ->
    Map = map_data:create (standard_game),
    map:add_unit (Map, {fleet, austria}, albania),
    map:add_unit (Map, {army, austria}, serbia),
    map:add_unit (Map, {army, austria}, ukraine),
    map:add_unit (Map, {fleet, austria}, north_africa),
    ?debugMsg ("counting:"),
    rules:process (count_phase, Map, diplomacy_rules, []),
    %% austria has now 4 supply centers (home country and serbia),
    %% but 7 units (home country + A ukraine,
    %%                             A serbia,
    %%                             F albania,
    %%                             F north_africa)
    %% this means three have to go
    rules:process (build_phase, Map, diplomacy_rules, []),
    %% oops, no destroy orders! --> civil disorder rule came into affect!

    %% Removed where, according to the rules:
    %%  1) F north_africa (furthest away)
    %%  2) F albania (equally far away but it's a fleet)
    %%  3) A serbia (as far away as ukraine but first in alphabetical order)
    ?assertEqual ([], map:get_units (Map, north_africa)),
    ?assertEqual ([], map:get_units (Map, albania)),
    ?assertEqual ([], map:get_units (Map, serbia)),
    ?assertEqual ([{army, austria}], map:get_units (Map, ukraine)),
    map_data:delete (Map).

