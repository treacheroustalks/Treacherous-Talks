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
%%% @doc Tests the logout in the controller.
%%% 
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(logout_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId))
    ].

%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    ?debugMsg("LOGOUT TEST SUCCESS"),
    {ok, User} = session:get_session_user(SessId, no_arg),

    Cmd = {logout, {ok, SessId, []}},
    Result = controller:handle_action(Cmd, Callback),

    ?assertMatch({{logout, success}, ok}, Result),

    PresenceResult = session_presence:is_online(User#user.id),
    ?assertEqual(true, PresenceResult),
    ?debugMsg("LOGOUT TEST SUCCESS finished").
