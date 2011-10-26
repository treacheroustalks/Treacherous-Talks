-module (db_obj_test).

-include_lib ("eunit/include/eunit.hrl").

db_obj_index_test() ->
    Bucket = <<"a_bucket">>,
    Key = <<"I am a key">>,
    Value = "awesome value",
    Obj = db_obj:create(Bucket, Key, Value),
    
    Index = <<"index">>,
    IndexKey = <<"index key">>,
    IndexTup = {Index, IndexKey},
    IndexList = [IndexTup],
    Obj2 = db_obj:add_index(Obj, IndexTup),
    ?debugVal(Obj2),
    ?assertEqual(IndexList, db_obj:get_indices(Obj2)),

    Obj3 = db_obj:remove_index(Obj2, IndexTup),
    ?debugVal(Obj3),
    ?assertEqual([], db_obj:get_indices(Obj3)),

    Obj4 = db_obj:set_indices(Obj3, IndexList),
    ?debugVal(Obj4),
    ?assertEqual(IndexList, db_obj:get_indices(Obj4)).
    
    
    
