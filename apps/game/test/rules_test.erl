-module (rules_test).

-include_lib ("eunit/include/eunit.hrl").

simple_move_test () ->
    Map = map_utils:create_map (standard_game),
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
                                         map_utils:unit_exists (Map,
                                                                MovedTo,
                                                                Unit))
                   end,
                   Orders),
    %%  and are gone where they were:
    lists:foreach (fun (Order={move, _, MovedFrom, _}) ->
                           ?debugVal (Order),
                           ?assertEqual ([], 
                                         map_utils:get_units (Map,
                                                              MovedFrom))
                   end,
                   Orders),
    map_utils:delete (Map).
