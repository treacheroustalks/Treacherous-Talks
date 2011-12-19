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
%%% @doc Test ACLs
%%
%%% @author Sukumar Yethadka <sbhat7@gmail.com>
%%
%%% @since : 15 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(tt_acl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId))
    ].
%%-------------------------------------------------------------------
%% Operator get db status test
%%-------------------------------------------------------------------
success(Callback, OperatorSessionId) ->
    ?debugMsg("TT ACL tests: Start"),
    % Start with three users - user, moderator and operator
    Moderator = create_user(moderator),
    User = create_user(user),
    {{login, success}, ModSessionId} =
        controller:handle_action(
          {login, {ok, {Moderator, controller_tests:get_receiver()}}}, Callback),
    {{login, success}, UserSessionId} =
        controller:handle_action(
          {login, {ok, {User, controller_tests:get_receiver()}}}, Callback),

    % Operator access check
    SysStatusCmd = {get_system_status, {ok, OperatorSessionId}},
    {SysStatusResult, _} = controller:handle_action(SysStatusCmd, Callback),
    ?assertEqual({get_system_status, success}, SysStatusResult),

    % Moderator access check
    ReportsCmd = {get_reports, {ok, ModSessionId, no_arg}},
    {ReportsResult, _} = controller:handle_action(ReportsCmd, Callback),
    ?assertEqual({get_reports, success}, ReportsResult),

    % User access check
    SessionUserCmd = {get_session_user, {ok, UserSessionId, no_arg}},
    {SessionUserResult, _} = controller:handle_action(SessionUserCmd, Callback),
    ?assertEqual({get_session_user, success}, SessionUserResult),
    ?debugMsg("TT ACL tests: Completed").

create_user(Role) ->
    User = controller_tests:create_user(),
    Register = {register, {ok, User#user{role = Role}}},
    controller:handle_action(Register,
                             {fun(_,_,Data) -> Data end, []}).
