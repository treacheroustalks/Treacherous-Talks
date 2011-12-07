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
%%% @doc Unit tests for login
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(login_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/user.hrl").

-export([
         tests/1,
         success/1,
         mult_nicks/1,
         user_not_existing/1,
         wrong_password/1,
         kill_old_session/1
        ]).

tests(Callback) ->
    [
     ?_test(success(Callback)),
     ?_test(mult_nicks(Callback)),
     ?_test(user_not_existing(Callback)),
     ?_test(wrong_password(Callback))
    ].
%%-------------------------------------------------------------------
%% Login tests
%%-------------------------------------------------------------------
success(Callback) ->
    ?debugMsg("LOGIN TEST SUCCESS"),
    User = get_test_data(success),
    Cmd = {login, {ok, {User, controller_tests:get_receiver()}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, SessionId} = Result,

    ?assertEqual({login, success}, CmdRes),

    SessResult = session:alive(SessionId),
    ?assertEqual(true, SessResult),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(true, PresenceResult),
    ?debugMsg("LOGIN TEST SUCCESS finished").

mult_nicks(Callback) ->
    ?debugMsg("LOGIN TEST MULT_NICKS"),
    User = get_test_data(mult_nicks),
    Cmd = {login, {ok, {User, controller_tests:get_receiver()}}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(nick_not_unique, Info),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(false, PresenceResult),
    ?debugMsg("LOGIN TEST MULT_NICKS finished").

user_not_existing(Callback) ->
    ?debugMsg("LOGIN TEST USER_NOT_EXISTING"),
    User = get_test_data(user_not_existing),
    Cmd = {login, {ok, {User, controller_tests:get_receiver()}}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(invalid_login_data, Info),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(false, PresenceResult),
    ?debugMsg("LOGIN TEST USER_NOT_EXISTING finished").

wrong_password(Callback) ->
    ?debugMsg("LOGIN TEST WRONG_PASSWORD"),
    User = get_test_data(wrong_password),
    Cmd = {login, {ok, {User, controller_tests:get_receiver()}}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(invalid_login_data, Info),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(false, PresenceResult),
    ?debugMsg("LOGIN TEST WRONG_PASSWORD finished").

kill_old_session(Callback) ->
    ?debugMsg("LOGIN TEST KILL_OLD_SESSION"),
    {User, Session} = get_test_data(kill_old_session),
    Cmd = {login, {ok, {User, controller_tests:get_receiver()}}},
     Result = controller:handle_action(Cmd, Callback),
    {CmdRes, SessionId} = Result,
    AliveResult = session:alive(SessionId),
    ?assertEqual(true, AliveResult),

    ?assertEqual({login, success}, CmdRes),

    MonitorRef = monitor(process, session_id:to_pid(Session)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end,
    SessResult = session:alive(Session),
    ?assertEqual(false, SessResult),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(true, PresenceResult),
    ?debugMsg("LOGIN TEST KILL_OLD_SESSION finished").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    User = controller_tests:create_user(),
    Register = {register, {ok, User}},
    controller:handle_action(Register,
                             {fun(_,_,Data) -> Data end, []});
get_test_data(mult_nicks) ->
    % create artificial scenario of multiple entries for one nick
    User = controller_tests:create_user(),

    Id1 = db:get_unique_id(),
    User1 = User#user{id = Id1},
    BinId1 = db:int_to_bin(Id1),
    UserPropList1 = data_format:rec_to_plist(User1),
    DbObj1 = db_obj:create(?B_USER, BinId1, UserPropList1),
    db:put(DbObj1),

    Id2 = db:get_unique_id(),
    User2 = User#user{id = Id2},
    BinId2 = db:int_to_bin(Id2),
    UserPropList2 = data_format:rec_to_plist(User2),
    DbObj2 = db_obj:create(?B_USER, BinId2, UserPropList2),
    db:put(DbObj2),

    User;
get_test_data(user_not_existing) ->
    #user{nick = "u9hvawebf802bv82 ",
          password = "jksbnaugh29 RANDOM"};
get_test_data(wrong_password) ->
    User = get_test_data(success),
    User#user{password = User#user.password ++ "-"};
get_test_data(kill_old_session) ->
    User = get_test_data(success),
    Cmd = {login, {ok, User}},
    Session = controller:handle_action(
                Cmd,{fun(_,_,Data) -> Data end, []}),
    {User, Session}.

