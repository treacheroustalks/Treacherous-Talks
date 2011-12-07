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
-module(db_test).

-include_lib("eunit/include/eunit.hrl").

-export([create_siblings/0]).

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
      ?_test(list_keys()),
      ?_test(get_resolve()),
      ?_test(get_db_stats()),
      ?_test(get_values())
     ]
    }.

%% ping_riak test
ping_riak() ->
    Result = db:ping_riak(),
    ?assertEqual(pong, Result).

%% get_db_stats test
get_db_stats() ->
    {Reply,_}=db:get_db_stats(),
    ?assertEqual(ok, Reply).

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


%% list_keys test
list_keys() ->
    Bucket = <<"list_keys_test">>,
    Key = <<"some key">>,
    Val = a_value,
    Obj = db_obj:create(Bucket, Key, Val),

    db:put(Obj),

    Result = db:list_keys(Bucket),
    ?assertEqual({ok, [Key]}, Result),
    db:delete(Bucket, Key).

%% put/get test
get_resolve() ->
    {Bucket, Key, _InitVal, Sib1Val, Sib2Val,
     LastSessField, Hist, ReverseHist, _DbObj} =
        db_test:create_siblings(),

    {ok, ResultObj} = db:get_resolve(Bucket, Key, Hist, LastSessField),
    ?assertEqual(Sib2Val, db_obj:get_value(ResultObj)),

    {ok, ReverseResultObj} = db:get_resolve(Bucket, Key, ReverseHist, LastSessField),
    ?assertEqual(Sib1Val, db_obj:get_value(ReverseResultObj)).

%% put/get test
get_values() ->
    Bucket = <<"get_list_test">>,
    {ok, OldKeys} = db:list_keys(Bucket),
    lists:foreach(fun(K) ->
                          db:delete(Bucket, K)
                  end, OldKeys),
    % create some values in the db
    Count = 10,
    {Keys, Values} = lists:foldl(fun(No, {CurKeys, CurValues}) ->
                                         Key = db:int_to_bin(
                                                 db:get_unique_id()),
                                         Val = {val, No},
                                         Obj = db_obj:create(Bucket, Key, Val),
                                         db:put(Obj),
                                         {[Key|CurKeys], [Val|CurValues]}
                                 end, {[], []}, lists:seq(1, Count)),

    Result = db:get_values(Bucket, Keys),
    ?assertMatch({ok, _Vals}, Result),
    {ok, ResValues} = Result,
    ?assertEqual(Count, length(ResValues)),
    lists:foreach(fun(Val) ->
                          ?assertEqual(true, lists:member(Val, ResValues))
                  end, Values).



%% test int_to_bin
int_to_bin_test() ->
    Id = 123456789,
    ?assertEqual(<<"123456789">>, db:int_to_bin(Id)),

    ?assertEqual(<<"123456789-suffix">>, db:int_to_bin(Id, "-suffix")).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
-record(conflict, {val, last_session}).

create_siblings() ->
    % Create values
    Bucket = <<"some_conflict_bucket">>,
    Id = db:get_unique_id(),
    Key = db:int_to_bin(Id),

    Sess1 = "sess1",
    Sess2 = "sess2",
    Hist = session_history:add(
             session_history:add(
               session_history:create(Id),
               Sess1),
             Sess2),
    ReverseHist = session_history:add(
                    session_history:add(
                      session_history:create(Id),
                      Sess2),
                    Sess1),

    InitVal = #conflict{val = init_val},
    Sib1Val = #conflict{val = sib_1, last_session = Sess1},
    Sib2Val = #conflict{val = sib_2, last_session = Sess2},

    % create initial db value
    db:set_bucket(Bucket, [{allow_mult, true}]),

    InitObj = db_obj:create(Bucket, Key, InitVal),
    db:put(InitObj),

    {ok, DbObj} = db:get(Bucket, Key),

    Sib1Obj = db_obj:set_value(DbObj, Sib1Val),
    Sib2Obj = db_obj:set_value(DbObj, Sib2Val),

    db:put(Sib1Obj),
    db:put(Sib2Obj),

    % check we really got sibligns
    {ok, ReadObj} = db:get(Bucket, Key),
    ?assertEqual(true, db_obj:has_siblings(ReadObj)),

    {Bucket, Key, InitVal, Sib1Val, Sib2Val,
     #conflict.last_session, Hist, ReverseHist, ReadObj}.
