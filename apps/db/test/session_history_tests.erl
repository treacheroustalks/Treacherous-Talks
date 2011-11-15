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
%%% @doc Unit tests for the session history interface.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_history_tests).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [protobuffs, riakc, db].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% All session history interface tests
%%-------------------------------------------------------------------
session_history_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [
      ?_test(history_create()),
      ?_test(history_add_get_latest()),
      ?_test(history_db()),
      ?_test(history_find_newest()),
      ?_test(resolve_conflict())
     ]
    }.

%%-------------------------------------------------------------------
%% Testing history creation
%%-------------------------------------------------------------------
history_create() ->
    Hist = get_history(),
    Latest = session_history:latest(Hist),

    ?assertEqual(history_empty, Latest),

    session_history:delete(Hist).

%%-------------------------------------------------------------------
%% Testing adding of sessions to the history and getting the latest
%%-------------------------------------------------------------------
history_add_get_latest() ->
    Sessions = get_sessions(5),
    Hist = get_history(),

    LastHistory = lists:foldl(fun(Session, History) ->
                                     NewHistory = session_history:add(History, Session),
                                     Latest = session_history:latest(NewHistory),
                                     ?assertEqual({ok, Session}, Latest),
                                     NewHistory
                             end, Hist, Sessions),

    session_history:delete(LastHistory).

%%-------------------------------------------------------------------
%% Testing history creation
%%-------------------------------------------------------------------
history_db() ->
    Hist1 = get_history(),
    Hist2 = lists:foldl(fun(Session, Hist) ->
                                session_history:add(Hist, Session)
                        end, Hist1, get_sessions(5)),

    WriteResult = session_history:db_put(Hist2),
    ?assertEqual(ok, WriteResult),

    {ok, ReadResult} = session_history:db_get(session_history:id(Hist2)),
    ?assertEqual(Hist2, db_obj:get_value(ReadResult)),

    Hist3 = session_history:add(Hist2, "sdfad"),
    NewObj = db_obj:set_value(ReadResult, Hist3),
    UpdateResult = session_history:db_update(NewObj),
    ?assertEqual(ok, UpdateResult).

%%-------------------------------------------------------------------
%% Testing adding of sessions to the history and getting the latest
%%-------------------------------------------------------------------
history_find_newest() ->
    Sessions = get_sessions(5),
    Hist = get_history(),

    Hist2 = lists:foldl(fun(Session, History) ->
                                session_history:add(History, Session)
                        end, Hist, Sessions),

    Newest = session_history:find_newest(Hist2, Sessions),
    ?assertEqual(5, Newest),

    ReverseNewest = session_history:find_newest(Hist2, lists:reverse(Sessions)),
    ?assertEqual(1, ReverseNewest),

    NewestFail = session_history:find_newest(Hist2, ["abc"]),
    ?assertEqual(not_in_history, NewestFail),

    NewestEmpty = session_history:find_newest(get_history(), Sessions),
    ?assertEqual(history_empty, NewestEmpty).


resolve_conflict() ->
    {_Bucket, _Key, _InitVal, Sib1Val, Sib2Val,
     LastSessField, Hist, ReverseHist, DbObj} =
        db_test:create_siblings(),

    % perform the resolution test
    ResultObj = session_history:resolve_conflict(
                  Hist, DbObj, LastSessField),
    ResultVal = db_obj:get_value(ResultObj),
    ?assertEqual(Sib2Val, ResultVal),

    ReverseResultObj = session_history:resolve_conflict(
                         ReverseHist, DbObj, LastSessField),
    ReverseResultVal = db_obj:get_value(ReverseResultObj),
    ?assertEqual(Sib1Val, ReverseResultVal).


%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_history() ->
    session_history:create(db:get_unique_id()).

get_sessions(Count) ->
    lists:map(fun(No) ->
                      "JustSomeString" ++ integer_to_list(No)
              end, lists:seq(1, Count)).
