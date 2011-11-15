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
    Cmd = {login, {ok, get_test_data(success)}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, SessionId} = Result,

    ?assertEqual({login, success}, CmdRes),

    SessResult = session:alive(SessionId),
    ?assertEqual(true, SessResult).

mult_nicks(Callback) ->
    User = get_test_data(mult_nicks),
    Cmd = {login, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(nick_not_unique, Info).

user_not_existing(Callback) ->
    User = get_test_data(user_not_existing),
    Cmd = {login, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(invalid_login_data, Info).

wrong_password(Callback) ->
    User = get_test_data(wrong_password),
    Cmd = {login, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(invalid_login_data, Info).

kill_old_session(Callback) ->
    {User, Session} = get_test_data(kill_old_session),
    Cmd = {login, {ok, User}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, SessionId} = Result,
    AliveResult = session:alive(SessionId),
    ?assertEqual(true, AliveResult),

    ?assertEqual({login, success}, CmdRes),
    
    MonitorRef = monitor(process, session_id:to_pid(Session)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end,
    SessResult = session:alive(Session),
    ?assertEqual(false, SessResult).

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
    DbObj1 = db_obj:add_index(
               db_obj:create(?B_USER, BinId1, User1),
               {<<"nick_bin">>, list_to_binary(User#user.nick)}),
    db:put(DbObj1),

    Id2 = db:get_unique_id(),
    User2 = User#user{id = Id2},
    BinId2 = db:int_to_bin(Id2),
    DbObj2 = db_obj:add_index(
               db_obj:create(?B_USER, BinId2, User2),
               {<<"nick_bin">>, list_to_binary(User#user.nick)}),
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
    
