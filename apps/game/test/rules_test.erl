-module (rules_test).

-include_lib ("eunit/include/eunit.hrl").

simple_move_test () ->
    Map = map_data:create (standard_game),
    Orders = [{move, {army, germany}, berlin, prussia},
              {move, {army, austria}, vienna, galicia},
              {move, {fleet, italy}, napoli, tyrhennian_sea}],
    lists:all (fun (Reply) ->
                       case Reply of
                           {executed, _} -> 
                               true;
                           NotExecuted -> 
                               ?debugVal (NotExecuted),
                               false
                       end
               end,
               rules:process (spring, Map, Orders)),
    %% check if they are, where they should be
    lists:foreach (fun (Order={move, Unit, _, MovedTo}) ->
                           ?debugVal (Order),
                           ?assertEqual (true, 
                                         map:unit_exists (Map,
                                                                MovedTo,
                                                                Unit))
                   end,
                   Orders),
    %%  and are gone where they were:
    lists:foreach (fun (Order={move, _, MovedFrom, _}) ->
                           ?debugVal (Order),
                           ?assertEqual ([], 
                                         map:get_units (Map,
                                                              MovedFrom))
                   end,
                   Orders),
    map_data:delete (Map).
