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
%%% @doc Unit tests for getting user presence
%%% @end
%%%
%%% @since : 7 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_presence_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/3]).

tests([Callback, OnlineUsers, OfflineUsers]) ->
    [
     ?_test(success(Callback, OnlineUsers, OfflineUsers))
    ].
%%-------------------------------------------------------------------
%% User presence tests
%%-------------------------------------------------------------------
success(Callback, OnlineUsers, OfflineUsers) ->
    ?debugMsg("Get Presence test: START"),
    [CurrentUserId, User1Id] = OnlineUsers,
    User2Id = hd(OfflineUsers),

    {ok, SessionId} = session_presence:get_session_id(CurrentUserId),
    User1 = user_management:get(User1Id),
    User2 = user_management:get(User2Id),

    UserCmd1 = {get_presence, {ok, SessionId, User1#user.nick}},
    Result1 = controller:handle_action(UserCmd1, Callback),
    {CmdRes1, Data1} = Result1,
    ?assertEqual({get_presence, success}, CmdRes1),
    ?assertEqual(user_online, Data1),

    UserCmd2 = {get_presence, {ok, SessionId, User2#user.nick}},
    Result2 = controller:handle_action(UserCmd2, Callback),
    {CmdRes2, Data2} = Result2,
    ?assertEqual({get_presence, success}, CmdRes2),
    ?assertEqual(user_offline, Data2),

    UserCmd3 = {get_presence, {ok, SessionId, get_unique_nick()}},
    Result3 = controller:handle_action(UserCmd3, Callback),
    {CmdRes3, Data3} = Result3,
    ?assertEqual({get_presence, invalid_data}, CmdRes3),
    ?assertEqual(user_not_found, Data3),

    ?debugMsg("Get Presence test: STOP").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_unique_nick() ->
    "uniqueNick" ++ integer_to_list(db:get_unique_id()).