-module(db_test).

-include_lib("eunit/include/eunit.hrl").


%% startup
apps() ->
    [service, protobuffs, riakc, db].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

%% teardown
app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).


%% testing the db public interface
db_put_get_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [
      ?_test(ping_riak()),
      ?_test(delete()),
      ?_test(put_get()),
      ?_test(get_index()),
      ?_test(list_keys()),
      ?_test(get_index_with_siblings())
     ]
    }.

%% ping_riak test
ping_riak() ->
    Result = db:ping_riak(),
    ?assertEqual(pong, Result).

%% put/get test
put_get() ->
    Bucket = <<"put_get_test">>,
    Key = <<"some key">>,
    Val = a_value,
    Obj = db_obj:create(Bucket, Key, Val),
    
    db:put(Obj),
    
    {ok, ResObj} = db:get(Bucket, Key),
    ?assertEqual(Val, db_obj:get_value(ResObj)),
    db:delete(Bucket, Key).
    
%% delete test
delete() ->
    Bucket = <<"delete_test">>,
    Key = <<"some key">>,
    Val = a_value,
    Obj = db_obj:create(Bucket, Key, Val),
    
    db:put(Obj),
    db:delete(Bucket, Key),

    Result = db:get(Bucket, Key),
    ?assertEqual({error, notfound}, Result).
    
%% get_index test
get_index() ->
    Bucket = <<"get_index_test">>,
    Key = <<"some key">>,
    Val = a_value,
    Obj = db_obj:create(Bucket, Key, Val),

    Idx = {<<"index_bin">>, term_to_binary(Val)},
    Obj2 = db_obj:add_index(Obj, Idx),
    
    db:put(Obj2),
    
    Result = db:get_index(Bucket, Idx),
    ?assertEqual({ok, [[Bucket, Key]]}, Result),
    db:delete(Bucket, Key).
    
%% get_index test
list_keys() ->
    Bucket = <<"list_keys_test">>,
    Key = <<"some key">>,
    Val = a_value,
    Obj = db_obj:create(Bucket, Key, Val),
    
    db:put(Obj),
    
    Result = db:list_keys(Bucket),
    ?assertEqual({ok, [Key]}, Result),
    db:delete(Bucket, Key).
    
%% get_index test
get_index_with_siblings() ->
    Bucket = <<"index_siblings_test">>,
    db:set_bucket(Bucket, [{allow_mult, true}]),
    Id = db:get_unique_id(),
    Key = db:int_to_bin(Id),
    Val = {just, a_value},
    Obj = db_obj:create(Bucket, Key, Val),

    Idx = <<"index_bin">>,
    IdxVal = list_to_binary("first_idx" ++ integer_to_list(Id)),
    Obj2 = db_obj:add_index(Obj, {Idx, IdxVal}),

    db:put(Obj2),

    % created, now we get the object and create siblings
    {ok, DbObj} = db:get(Bucket, Key),

    IdxVal2 = list_to_binary("second_idx" ++ integer_to_list(Id)),
    DbObj2 = db_obj:set_indices(DbObj, [{Idx, IdxVal2}]),
    IdxVal3 = list_to_binary("third_idx" ++ integer_to_list(Id)),
    DbObj3 = db_obj:set_indices(DbObj, [{Idx, IdxVal3}]),
    db:put(DbObj2),
    db:put(DbObj3),

    {ok, Result1} = db:get_index(Bucket, {Idx, IdxVal}),
    {ok, Result2} = db:get_index(Bucket, {Idx, IdxVal2}),
    {ok, Result3} = db:get_index(Bucket, {Idx, IdxVal3}),
    ?assertEqual([], Result1),
    ?assertEqual([[Bucket, Key]], Result2),
    ?assertEqual([[Bucket, Key]], Result3),

    {ok, DbSib} = db:get(Bucket, Key),
    [Sibling|_] = db_obj:get_siblings(DbSib),
    ?assertEqual(Val, db_obj:get_value(Sibling)),
    db:delete(Bucket, Key).

%% test int_to_bin
int_to_bin_test() ->
    Id = 123456789,
    ?assertEqual(<<"123456789">>, db:int_to_bin(Id)),

    ?assertEqual(<<"123456789-suffix">>, db:int_to_bin(Id, "-suffix")).
