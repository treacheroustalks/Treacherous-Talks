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
%%% @doc Unit tests for updating user
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(assign_moderator_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1]).

tests(Callback) ->
    [
     ?_test(success_add(Callback)),
     ?_test(access_denied(Callback)),
     ?_test(user_not_found(Callback))
    ].
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success_add(Callback) ->
    ?debugMsg("ASSIGN MODERATOR TEST SUCCESS"),
    Operator = get_test_operator(success),
    User = controller_tests:create_user(),
    SessId = register_and_login(Operator),
    register_and_login(User),
    Cmd = {assign_moderator, {ok, SessId, {User#user.nick, add}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,
    ?assertEqual({assign_moderator, success}, CmdRes),
    ?debugMsg("ASSIGN MODERATOR TEST SUCCESS finished").


access_denied(Callback) ->
    ?debugMsg("ASSIGN MODERATOR TEST ACCESS_DENIED"),
    FakeOperator = get_test_operator(invalid),
    User = controller_tests:create_user(),
    register_and_login(User),
    SessId = register_and_login(FakeOperator),
    Cmd = {assign_moderator, {ok, SessId, {User#user.nick, add}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,
    ?assertEqual({assign_moderator, access_denied}, CmdRes),
    ?debugMsg("ASSIGN MODERATOR TEST ACCESS DENIED finished").

user_not_found(Callback) ->
    ?debugMsg("ASSIGN MODERATOR TEST USER NOT FOUND"),
    Operator = get_test_operator(success),
    SessId = register_and_login(Operator),
    InvalidNick = "invalid" ++ integer_to_list(db_c:get_unique_id()) ++ "nick",
    Cmd = {assign_moderator, {ok, SessId, {InvalidNick, add}}},
    Result = controller:handle_action(Cmd, Callback),
    CmdRes = Result,
    ?assertEqual({{assign_moderator, invalid_data}, user_not_found}, CmdRes),
    ?debugMsg("ASSIGN MODERATOR TEST USER NOT FOUND finished").
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_operator(success) ->
    controller_tests:create_operator();
get_test_operator(invalid) ->
    controller_tests:create_user().

register_and_login(User) ->
    Reply = {fun(_,_,Data) -> Data end, []},
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, controller_tests:get_receiver()}}},
    controller:handle_action(Login, Reply).
