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

%% manual diagram 6
diagram_6_test () ->
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

%% manual diagram 7 - in a different location, for convenience
diagram_7_test () ->
    Map = map_data:create (standard_game),
    rules:process (order_phase, Map, diplomacy_rules,
                       [{move, {fleet, austria}, trieste, venice},
                        {move, {army, italy}, venice, trieste}]),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
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
%    ?debugMsg ("###################################### DIAGRAM 12"),
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
    Response =
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
%    ?assert (lists:member ({dislodge, {army, germany}, munich}, Response)),
    ?assert (map:unit_exists (Map, munich, {army, germany})),
    ?assert (map:unit_exists (Map, munich, {army, austria})),
    ?assert (map:unit_exists (Map, prussia, {army, russia})),
    ?assert (map:unit_exists (Map, warsaw, {army, russia})),
    ?assert (map:unit_exists (Map, tyrolia, {army, austria})),
%    ?debugMsg ("###################################### DIAGRAM 12: DONE"),
    map_data:delete (Map).
