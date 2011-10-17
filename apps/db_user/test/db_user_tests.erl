-module(db_user_tests).

-include_lib("eunit/include/eunit.hrl").

db_code_loaded_test() ->
    ?debugVal (code:which (db_c)),
    ?assert (non_existing =/= code:which (db_c)),
    ?debugVal (code:which (riakc_pb_socket)),
    ?assert (non_existing =/= code:which (riakc_pb_socket)),
    ok.

%% --------------------------------------------------
%% "application" specific use case
%% --------------------------------------------------

connect_setup () ->
    ?debugMsg ("connecting.."),
    application:start (db_user), % this shouldn't be necessary. but it is.
    ?debugVal (application:get_env (db_user,riak)),
    {ok, Riak} = application:get_env (db_user,riak),

    {ok, Client} = db_c:connect (Riak),
    Client.

connect_teardown (Client) ->
    ?debugMsg ("disconnecting.."),
    db_c:disconnect (Client).

db_connect_test_ () ->
    {setup,
     fun connect_setup/0,
     fun connect_teardown/1,
     [fun () -> ?debugMsg ("connected") end]}.

db_crud_test_ () ->
    {"write,read,delete-test",
     {setup,
      fun connect_setup/0,
      fun connect_teardown/1,
      fun db_write_read_delete_instantiator/1}}.

db_write_read_delete_instantiator (Client) ->
    fun () ->
            ?debugMsg ("message"),
            ?debugVal (Client),
            Item={["item string!", item_atom, {item_tuple}], whoah},
            DBItem=db_obj:create (<<"bucket">>, <<"key">>, Item),
            ?debugVal (DBItem),
            db_c:put (Client,DBItem),
            {ok,ReadItem} = db_c:get (Client, <<"bucket">>, <<"key">>),
            ?debugVal (ReadItem),
            ?debugVal (db_obj:get_value (ReadItem)),
            Item=db_obj:get_value (ReadItem)
    end.
