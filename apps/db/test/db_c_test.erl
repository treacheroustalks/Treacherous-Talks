-module (db_c_test).

-include_lib ("eunit/include/eunit.hrl").

connected_startup () ->
    {ok, Client} = db_c:connect ({pb, {"127.0.0.1", 8081}}),
    Client.

connected_teardown (Client) ->
    db_c:disconnect (Client).

many_concurrent_conns_test () ->
    Clients = lists:map (fun (N) ->
                                 Client = connected_startup (),
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

write_siblings (Client) ->
    [fun () ->
             db_c:set_bucket (Client, <<"test">>, [{allow_mult, true}]),
             Obj1 = db_obj:create (<<"test">>, <<"key">>, {sibling1}),
             ?debugVal (Obj1),
             db_c:put (Client, Obj1),
             Obj2 = db_obj:create (<<"test">>, <<"key">>, {sibling2}),
             ?debugVal (Obj2),
             db_c:put (Client, Obj2),
             {ok, Siblings}=db_c:get (Client,<<"test">>,<<"key">>),
             ?debugVal (db_obj:get_siblings (Siblings)),
             db_c:delete (Client, <<"bucket">>, <<"key">>)
     end].

siblings_test_ () ->
    {setup,
     fun connected_startup/0,
     fun connected_teardown/1,
     fun write_siblings/1}.

write_undefined_test_ () ->
    {setup,
     fun connected_startup/0,
     fun connected_teardown/1,
     fun write_undefined_tst_/1}.

write_undefined_tst_ (Client) ->
    [fun () ->
             DBObj = db_obj:create (<<"test">>, undefined, {test, item}),
             ?debugVal (DBObj),
             {ok, Key} = db_c:put (Client, DBObj),
             ?debugVal (Key),
             {ok, ReadDBItem} = db_c:get (Client, <<"test">>, Key),
             ReadItem = db_obj:get_value (ReadDBItem),
             ?debugVal (ReadItem),
             ReadItem = {test, item}
     end].
