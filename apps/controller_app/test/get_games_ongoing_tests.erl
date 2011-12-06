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
%%% @author Sukumar Yethadka <sbhat7@gmail.com>
%%%
%%% @doc Unit tests for getting ongoing games for the operator panel
%%% @end
%%%
%%% @since : 06 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_games_ongoing_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([tests/1, success/1]).

tests([Callback, SessId]) ->
    [?_test(success([Callback, SessId]))].
%%-------------------------------------------------------------------
%% games search tests
%%-------------------------------------------------------------------
success([Callback, SessId]) ->
    ?debugMsg("Get games ongoing: Success: Start"),
    Data = dummy,
    Cmd = {get_games_ongoing, {ok, SessId, Data}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Games} = Result,
    ?assertEqual({get_games_ongoing, success}, CmdRes),
    ?debugMsg("Get games ongoing: Success: Completed").
