%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for the controller worker
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/user.hrl").

%% startup
apps() ->
    [protobuffs, riakc, db, datatypes, service, controller_app].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

%% teardown
app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).


%% testing the session interface
controller_worker_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [
      ?_test(resolve_user_conflict())
     ]
    }.


resolve_user_conflict() ->
    % create initial db value
    Bucket = <<"user_test_bucket">>,
    db:set_bucket(Bucket, [{allow_mult, true}]),

    Id = db:get_unique_id(),
    Key = db:int_to_bin(Id),
    User = (controller_tests:create_user())#user{id = Id},
    Obj = db_obj:create(Bucket, Key, User),
    db:put(Obj),

    % create siblings
    Sess2 = "abcd",
    Sess3 = "dcba",
    Hist = session_history:add(
             session_history:add(
               session_history:create(Id),
               Sess2),
             Sess3),

    {ok, DbObj} = db:get(Bucket, Key),
    User2 = User#user{name = Sess2, last_session = Sess2},
    User3 = User#user{name = Sess3, last_session = Sess3},

    DbObj2 = db_obj:set_value(DbObj, User2),
    DbObj3 = db_obj:set_value(DbObj, User3),

    db:put(DbObj2),
    db:put(DbObj3),

    % check we really got sibligns
    {ok, ReadObj} = db:get(Bucket, Key),
    ?assertEqual(true, db_obj:has_siblings(ReadObj)),

    % perform the resolution test
    ResultObj = controller_app_worker:resolve_user_conflict(Hist, ReadObj),
    ResultVal = db_obj:get_value(ResultObj),
    ?assertEqual(User3, ResultVal).
