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



secondary_indices_test_ () ->
    {setup,
     fun connected_startup/0,
     fun connected_teardown/1,
     fun write_index/1}.


-record(index_test, {id,
                     nick = "bob",
                     other_stuff = "Blaaaaaaaaaaaaa"}).

get_index_test_record() ->
    #index_test{id = list_to_binary(integer_to_list(db_c:get_unique_id()))}.

write_index(Client) ->
    [fun() ->
             Bucket = <<"index_test">>,
             db_c:set_bucket (Client, Bucket, [{allow_mult, true}]),
             {ok, Keys} = db_c:list_keys(Client, Bucket),
             lists:foreach(fun(K) ->
                               db_c:delete(Client, Bucket, K)
                       end, Keys),

             Val = get_index_test_record(),
             Key = Val#index_test.id,
             Obj1 = db_obj:create (Bucket, Key, Val),
             ?debugVal (Obj1),

             Index = <<"nick_bin">>,
             IndexKey = list_to_binary(Val#index_test.nick),
             Obj2 = db_obj:add_index(Obj1, {Index, IndexKey}),
             ?debugVal(Obj2),
             db_c:put(Client, Obj2),

             {ok, GetObj} = db_c:get (Client, Bucket, Key),
             ?debugVal(GetObj),
             ?assertEqual(Val, db_obj:get_value(GetObj)),
             ?assertEqual([{Index, IndexKey}], db_obj:get_indices(GetObj)),

             IdxResult = db_c:get_index(Client, Bucket, {Index, IndexKey}),
             ?debugVal(IdxResult),
             {ok, GetIdx} = IdxResult,
             ?debugVal(GetIdx),
             ?assertEqual([ [Bucket, Key] ], GetIdx),
             db_c:delete(Client, Bucket, Key)
     end].
