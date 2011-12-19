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
%%% @doc Tests for the event pushing.
%%%
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(push_events_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId)),
     ?_test(invalid(Callback, SessId))
    ].

%%-------------------------------------------------------------------
%% Push an event
%%-------------------------------------------------------------------
success(_Callback, SessId) ->
    ?debugMsg("PUSH_EVENT TEST SUCCESS"),
    {ok, User} = session:get_session_user(SessId, no_arg),
    UserId = User#user.id,
    Event = #push_event{type = test_event,
                        data = {some, test, data}},

    % Asynchronous push event

    controller:push_event(UserId, Event),
    timer:sleep(100),
    Events1 = controller_tests:get_event(),
    ?assertEqual([Event], Events1),


    % Synchronous push event
    Result= controller:sync_push_event(UserId, Event),
    Events2 = controller_tests:get_event(),
    ?assertEqual([Event], Events2),

    ?assertEqual({ok, success}, Result).

invalid(_Callback, _SessId) ->
    ?debugMsg("PUSH_EVENT TEST INVALID"),
    UserId = db:get_unique_id(),

    Event = #push_event{type = test_event,
                        data = {some, test, data}},
    Result =controller:sync_push_event(UserId, Event),

    % check that user is really online
    ?assertEqual({error, not_online}, Result),

    ?debugMsg("PUSH_EVENT TEST SUCCESS finished").
