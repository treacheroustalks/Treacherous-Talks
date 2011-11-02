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

trade_places_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {fleet, austria}, trieste, venice},
                        {move, {army, italy}, venice, trieste}]),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    map_data:delete (Map).

unit_cannot_go_there_test () ->
    Map = map_data:create (standard_game),
    rules:process (something, Map, diplomacy_rules,
                   [{move, {army, austria}, budapest, tyrolia}]),
    ?assertEqual ([], map:get_units (Map, tyrolia)),
    ?assertEqual ([{army, austria}], map:get_units (Map, budapest)),
    map_data:delete (Map).

bounce_test () ->
    Map = map_data:create (standard_game),
    rules:process (something, Map, diplomacy_rules,
                   [{move, {army, austria}, vienna, galicia},
                    {move, {army, russia}, warsaw, galicia}]),
    ?assertEqual ([], map:get_units (Map, galicia)),
    map_data:delete (Map).

hold_vs_move_test () ->
    Map = map_data:create (standard_game),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    rules:process (something, Map, diplomacy_rules,
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
    ?assertEqual ([{{army, austria}, budapest}],
                  map:get_unit_info (Map, {army, austria}, vienna,
                                     supporting, no_support)),
    ?assertEqual (no_support,
                  map:get_unit_info (Map, {army, italy}, venice,
                                     supporting, no_support)),
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
%    ?debugMsg ("###################################### DIAGRAM 5"),
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
%    ?debugMsg ("###################################### DIAGRAM 5: DONE"),
    map_data:delete (Map).
