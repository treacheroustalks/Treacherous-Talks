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
    [mnesia, protobuffs, riakc, db, datatypes].

app_start() ->
    [ ?assertEqual (ok, application:start(App)) || App <- apps()],
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
       ?_test(session_presence_list_sessions()),
       ?_test(session_presence_list_sessions_by_client_type()),
       ?_test(session_presence_count_sessions()),
       ?_test(session_presence_count_sessions_by_client_type()),
       ?_test(session_presence_overwriting())
      ]
     }}.

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------
session_presence_get_session() ->
    UserId = get_test_id(),
    SessionId = "abcdefg",
    ClientType = mail,

    GetResult = session_presence:get_session_id(UserId),
    ?assertEqual({error, not_online}, GetResult),

    session_presence:add(UserId, SessionId, ClientType),
    GetResult2 = session_presence:get_session_id(UserId),
    ?assertEqual({ok, SessionId}, GetResult2),

    session_presence:remove(UserId, SessionId).

session_presence_add_remove() ->
    UserId = get_test_id(),
    SessionId = "abcdefg",
    ClientType = mail,

    AddResult = session_presence:add(UserId, SessionId, ClientType),
    ?assertEqual(ok, AddResult),
    GetResult = session_presence:is_online(UserId),
    ?assertEqual(true, GetResult),

    RmResult = session_presence:remove(UserId, SessionId),
    ?assertEqual(ok, RmResult),
    GetResult2 = session_presence:is_online(UserId),
    ?assertEqual(false, GetResult2),

    RmResult2 = session_presence:remove(UserId, SessionId),
    ?assertEqual({error, not_online}, RmResult2).


session_presence_list_sessions() ->
    Zipped = get_zipped_raw_data(),
    lists:map(fun({UserId, SessionId, ClientType}) ->
                      session_presence:add(UserId, SessionId, ClientType)
              end, Zipped),

    All = session_presence:get_all(),
    lists:map(fun(Val) ->
                      ?assertEqual(true, lists:member(Val, All))
              end, Zipped).


session_presence_list_sessions_by_client_type() ->
    Zipped = get_zipped_raw_data(),
    lists:map(fun({UserId, SessionId, ClientType}) ->
                      session_presence:add(UserId, SessionId, ClientType)
              end, Zipped),

    AllIm = session_presence:get_all_by_type(im),
    AllMail = session_presence:get_all_by_type(mail),
    AllWeb = session_presence:get_all_by_type(web),
    lists:map(fun(Val) ->
                  {Id, Session, Type} = Val,
                  case Type of
                      im  ->
                          ?assertEqual(true, lists:member({Id, Session}, AllIm));
                      mail ->
                          ?assertEqual(true, lists:member({Id, Session}, AllMail));
                      web ->
                          ?assertEqual(true, lists:member({Id, Session}, AllWeb))
                  end
              end, Zipped).


session_presence_count_sessions() ->
    Zipped = get_zipped_raw_data(),
    mnesia:clear_table(session),
    lists:map(fun({UserId, SessionId, ClientType}) ->
                      session_presence:add(UserId, SessionId, ClientType)
              end, Zipped),
    Count = session_presence:count_all(),
    ?assertEqual(10, Count).


session_presence_count_sessions_by_client_type() ->
    Zipped = get_zipped_raw_data(),
    mnesia:clear_table(session),
    lists:map(fun({UserId, SessionId, ClientType}) ->
                      session_presence:add(UserId, SessionId, ClientType)
              end, Zipped),
    CountIm = session_presence:count_all_by_type(im),
    CountMail = session_presence:count_all_by_type(mail),
    CountWeb = session_presence:count_all_by_type(web),
    ?assertEqual(3, CountIm),
    ?assertEqual(3, CountMail),
    ?assertEqual(4, CountWeb).

session_presence_overwriting() ->
    UserId = get_test_id(),
    SessionId1 = "abcdefg",
    SessionId2 = "abcdefg2",
    ClientType = mail,

    AddResult = session_presence:add(UserId, SessionId1, ClientType),
    GetResult = session_presence:get_session_id(UserId),
    ?assertEqual({ok, SessionId1}, GetResult),

    AddResult = session_presence:add(UserId, SessionId2, ClientType),
    GetResult2 = session_presence:get_session_id(UserId),
    ?assertEqual({ok, SessionId2}, GetResult2),

    RmResult = session_presence:remove(UserId, SessionId1),
    ?assertEqual({error, invalid_session}, RmResult).


%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_id() ->
    123456789.

get_zipped_raw_data() ->
    UserIds = lists:seq(1, 10),
    SessionIds = lists:map(fun integer_to_list/1, UserIds),
    ClientTypes = lists:map(fun(Id) ->
                                if
                                    Id >= 1,
                                    Id =< 3 -> im;
                                    Id >= 4,
                                    Id =< 6 -> mail;
                                    Id >= 7,
                                    Id =< 10 -> web
                                end
                            end, UserIds),
    lists:zip3(UserIds, SessionIds, ClientTypes).
