-module (db_c_test).

-include_lib ("eunit/include/eunit.hrl").

connected_startup () ->
    db_c:connect ({pb, {"127.0.0.1", 8081}}).

connected_teardown (Client) ->
    db_c:disconnect (Client).

many_concurrent_conns_test () ->
    Clients = lists:map (fun (N) ->
                                 {ok, Client} = connected_startup (),
                                 ?debugVal ({setup, N}),
                                 {N, Client}
                         end,
                         lists:seq (1,100)),
    ?debugMsg ("Many connections setup"),
    lists:map (fun ({N, Client}) -> 
                       connected_teardown (Client),
                       ?debugVal ({teardown, N})
               end,
               lists:reverse (Clients)).

     
