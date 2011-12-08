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
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc Unit tests for updating user
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(user_msg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/message.hrl").


-export([tests/1, success/2, invalid/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId)),
     ?_test(invalid(Callback, SessId))
    ].
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    ?debugMsg("USER_MSG TEST SUCCESS"),
    Data = create_valid_message(),
    Cmd = {user_msg, {ok, SessId, Data}},
    user_management:create(to_user()),

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,
    ?assertEqual({user_msg, success}, CmdRes),
    ?debugMsg("USER_MSG TEST SUCCESS finished").

invalid(Callback, SessId) ->
    ?debugMsg("USER_MSG TEST INVALID"),
    Data = create_invalid_message(),
    Cmd = {user_msg, {ok, SessId, Data}},
    Result = controller:handle_action(Cmd, Callback),
    ?assertEqual({{user_msg,invalid_data},invalid_nick}, Result),
    ?debugMsg("USER_MSG TEST INVALID finished").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
create_invalid_message() ->
      #frontend_msg{to = "user_withunderscore",
                    content = "\n\n    A sample message to nick player which
                              contain several line\n    have fun\n\n    "
                   }.

create_valid_message() ->
      #frontend_msg{to = "valid_user",
                    content = "\n\n    A sample message to nick player which
                              contain several line\n    have fun\n\n    "
                   }.

to_user_id() ->
    9999.

to_user() ->
    #user{id = to_user_id(), nick = "valid_user", email="sth@sth.pcs"}.
