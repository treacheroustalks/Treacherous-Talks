-module (rules_test).

-include_lib ("eunit/include/eunit.hrl").

simple_moves_test () ->
    Map = map_data:create (standard_game),
    OrdersAndResults = 
        [{ok, {move, {army, germany}, berlin, prussia}},
         {ok, {move, {army, austria}, vienna, galicia}},
         {ok, {move, {fleet, italy}, napoli, tyrhennian_sea}},
         {unit_does_not_exist, {move, {fleet, italy}, roma, napoli}}],
    {_, Orders} = lists:unzip (OrdersAndResults),
    ?assertEqual (OrdersAndResults, rules:process (spring, Map, 
                                                   Orders)),
    %% check if they are, where they should be
    lists:foreach (fun ({Result, _Order={move, Unit, _MovedFrom, MovedTo}}) ->
%                           ?debugVal (Order),
                           case Result of
                               ok ->
                                   ?assertEqual (true, 
                                                 map:unit_exists (Map,
                                                                  MovedTo,
                                                                  Unit));
                               _Other ->
                                   ?assertEqual (false,
                                                 map:unit_exists (Map,
                                                                  MovedTo,
                                                                  Unit))
                           end
                   end,
                   OrdersAndResults),
    map_data:delete (Map).
