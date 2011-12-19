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
%%% @doc Blacklist tests
%%
%%% @author Sukumar Yethadka <sbhat7@gmail.com>
%%
%%% @since : 16 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(blacklist_tests).

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
    ?debugMsg("Blacklist/Whitelist tests: Start"),
    % Create a user and log him in
    User = create_user(user),
    {{login, success}, _UserSessionId} =
        controller:handle_action(
          {login, {ok, {User, controller_tests:get_receiver()}}}, Callback),

    % Blacklist the user
    BlacklistCmd = {blacklist, {ok, OperatorSessionId, User#user.nick}},
    {BlacklistResult, BlUser} = controller:handle_action(BlacklistCmd, Callback),
    ?assertEqual({blacklist, success}, BlacklistResult),
    ?assertEqual(User#user{role = disabled}, BlUser#user{last_session=undefined}),

    % The blacklisted user should be now logged out
    PresenceCmd = {get_presence, {ok, OperatorSessionId, User#user.nick}},
    {PresenceResult, PresenceData} =
        controller:handle_action(PresenceCmd, Callback),
    ?assertEqual({get_presence, success}, PresenceResult),
    ?assertEqual(user_offline, PresenceData),

    % Login should now be disabled
    {LoginRes, LoginError} =
        controller:handle_action(
          {login, {ok, {User, controller_tests:get_receiver()}}}, Callback),
    ?assertEqual({login, invalid_data}, LoginRes),
    ?assertEqual(user_blacklisted, LoginError),

    % Whitelist the user
    WhitelistCmd = {whitelist, {ok, OperatorSessionId, User#user.nick}},
    {WhitelistResult, WlUser} = controller:handle_action(WhitelistCmd, Callback),
    ?assertEqual({whitelist, success}, WhitelistResult),
    ?assertEqual(User, WlUser#user{last_session=undefined}),

    % Login should now be enabled
    {NewLoginRes, _} =
        controller:handle_action(
          {login, {ok, {User, controller_tests:get_receiver()}}}, Callback),
    ?assertEqual({login, success}, NewLoginRes),

    ?debugMsg("Blacklist/Whitelist tests: Complete").

create_user(Role) ->
    User = controller_tests:create_user(),
    Register = {register, {ok, User#user{role = Role}}},
    controller:handle_action(Register,
                             {fun(_,_,Data) -> Data end, []}).
