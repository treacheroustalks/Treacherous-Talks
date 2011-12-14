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
%%% @author Sukumar Yethadka <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Tests for updating the push receiver of the session
%%%
%%% @end
%%%
%%% @since : 14 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(set_push_receiver_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId))
    ].

%%-------------------------------------------------------------------
%% Push an event
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    ?debugMsg("SET PUSH RECEIVER TEST SUCCESS"),
    ReceiverPid = whereis(receiver),
    {ok, User} = session:get_session_user(SessId, no_arg),
    UserId = User#user.id,
    Event = #push_event{type = test_event,
                        data = {some, test, data}},

    % Test current receiver is working
    controller:push_event(UserId, Event),
    timer:sleep(1),
    Events = controller_tests:get_event(),
    ?assertEqual([Event], Events),

    % Stop current receiver, create new receiver
    ReceiverPid ! stop,
    unregister(receiver),
    controller_tests:start_receiver(),
    NewReceiverPid = whereis(receiver),

    % Set the new push receiver
    Cmd = {set_push_receiver, {ok, SessId, controller_tests:get_receiver()}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,
    ?assertEqual({set_push_receiver, success}, CmdRes),

    % Test sending event with the new receiver
    controller:push_event(UserId, Event),
    timer:sleep(1),
    Events = controller_tests:get_event(),
    ?assertEqual([Event], Events),

    % Make sure that the new and old receivers are indeed different
    ?assertNot(ReceiverPid =:= NewReceiverPid),

    ?debugMsg("SET PUSH RECEIVER TEST SUCCESS finished").
