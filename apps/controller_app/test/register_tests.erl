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
%%% @doc Unit tests for the registration.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(register_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/1, invalid/1]).

tests(Callback) ->
    [
     ?_test(success(Callback)),
     ?_test(invalid(Callback))
    ].
%%-------------------------------------------------------------------
%% Register tests
%%-------------------------------------------------------------------
success(Callback) ->
    ?debugMsg("REGISTER TEST SUCCESS"),
    User = get_test_data(success),
    Cmd = {register, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, NewUser} = Result,
    ?assertEqual({register, success}, CmdRes),
    ?assertEqual(User#user{id = NewUser#user.id,
                           date_created=NewUser#user.date_created}, NewUser),
    ?debugMsg("REGISTER TEST SUCCESS finished").

invalid(Callback) ->
    ?debugMsg("REGISTER TEST INAVLID"),
    User = get_test_data(invalid),
    Cmd = {register, {ok, User}},

    {{register, success}, _User} = controller:handle_action(Cmd, Callback),
    % register again with that nick has to fail
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({register, invalid_data}, CmdRes),
    ?assertEqual(nick_already_exists, Info),
    ?debugMsg("REGISTER TEST INAVLID finished").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    controller_tests:create_user();
get_test_data(invalid) ->
    controller_tests:create_user().
