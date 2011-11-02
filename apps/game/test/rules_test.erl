-module (rules_test).

-include_lib ("eunit/include/eunit.hrl").

units_exist_test () ->
    Map = map_data:create (standard_game),
    Rules = diplomacy_rules:create (standard_game),
    %% there is no army in galicia!
    Response =
        rules:process (something, Map, Rules, [{move, {army, austria},
                                                 galicia, ukraine}]),
    ?assertEqual ([], Response),
    map_data:delete (Map).

trade_places_test () ->
    Map = map_data:create (standard_game),
    Rules = diplomacy_rules:create (standard_game),
    Response =
        rules:process (something, Map, Rules,
                       [{move, {army, austria}, budapest, vienna},
                        {move, {army, austria}, vienna, budapest}]),
    ?assertEqual ([], Response),
    map_data:delete (Map).

unit_cannot_go_there_test () ->
    Map = map_data:create (standard_game),
    Rules = diplomacy_rules:create (standard_game),
    Response =
        rules:process (something, Map, Rules,
                       [{move, {army, austria}, budapest, warsaw}]),
    ?assertEqual ([], Response),
    map_data:delete (Map).

bounce_test () ->
    Map = map_data:create (standard_game),
    Rules = diplomacy_rules:create (standard_game),
    rules:process (something, Map, Rules,
                   [{move, {army, austria}, vienna, galicia},
                    {move, {army, russia}, warsaw, galicia}]),
    ?assertEqual ([], map:get_units (Map, galicia)),
    map_data:delete (Map).

hold_vs_move_test () ->
%    ?debugMsg ("######################### DIPLOMACY-RULES TEST"),
    Map = map_data:create (standard_game),
    Rules = diplomacy_rules:create (standard_game),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
    rules:process (something, Map, Rules,
                   [{move, {fleet, austria}, trieste, venice},
                    {hold, {army, italy}, venice}]),
    ?assertEqual ([{fleet, austria}], map:get_units (Map, trieste)),
    ?assertEqual ([{army, italy}], map:get_units (Map, venice)),
%    ?debugMsg ("######################### DIPLOMACY-RULES TEST: DONE"),
    map_data:delete (Map).
