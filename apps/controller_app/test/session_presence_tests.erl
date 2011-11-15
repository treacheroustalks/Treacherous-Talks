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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Tests the session_presence interface.
%%% 
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_presence_tests).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [mnesia].

app_start() ->
    [ application:start(App) || App <- apps()],
    error_logger:tty(false).

app_stop(_) ->
    [ application:stop(App) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% Test preparation
%%-------------------------------------------------------------------
controller_test_() ->
    {timeout, 60,
     {setup,
      fun() ->
              app_start(),
              session_presence:init(),
              mnesia:clear_table(session)
      end,
      fun app_stop/1,
      [
       ?_test(session_presence_add_remove()),
       ?_test(session_presence_get_session()),
       ?_test(session_presence_list_sessions())
      ]
     }}.

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------
session_presence_get_session() ->
    UserId = get_test_id(),
    SessionId = "abcdefg",

    GetResult = session_presence:get_session_id(UserId),
    ?assertEqual({error, not_online}, GetResult),

    session_presence:add(UserId, SessionId),
    GetResult2 = session_presence:get_session_id(UserId),
    ?assertEqual({ok, SessionId}, GetResult2).


session_presence_add_remove() ->
    UserId = get_test_id(),
    SessionId = "abcdefg",

    AddResult = session_presence:add(UserId, SessionId),
    ?assertEqual(ok, AddResult),
    GetResult = session_presence:is_online(UserId),
    ?assertEqual(true, GetResult),

    RmResult = session_presence:remove(UserId),
    ?assertEqual(ok, RmResult),
    GetResult2 = session_presence:is_online(UserId),
    ?assertEqual(false, GetResult2),

    RmResult2 = session_presence:remove(UserId),
    ?assertEqual({error, not_online}, RmResult2).
    

session_presence_list_sessions() ->
    UserIds = lists:seq(1, 10),
    SessionIds = lists:map(fun integer_to_list/1, UserIds),
    Zipped = lists:zip(UserIds, SessionIds),
    lists:map(fun({UserId, SessionId}) ->
                      session_presence:add(UserId, SessionId)
              end, Zipped),

    All = session_presence:get_all(),
    lists:map(fun(Val) ->
                      ?assertEqual(true, lists:member(Val, All))
              end, Zipped).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_id() ->
    123456789.
