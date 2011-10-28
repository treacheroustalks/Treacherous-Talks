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
      ?_test(list_keys())
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
    

%% test int_to_bin
int_to_bin_test() ->
    Id = 123456789,
    ?assertEqual(<<"123456789">>, db:int_to_bin(Id)),

    ?assertEqual(<<"123456789-suffix">>, db:int_to_bin(Id, "-suffix")).
