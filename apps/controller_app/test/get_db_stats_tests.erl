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
%%% @doc Unit tests for operator.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_db_stats_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([tests/2, success/2, invalid/2]).

tests(Callback, SessId) ->
    [
     ?_test(success(Callback, SessId)),
     ?_test(invalid(Callback, SessId))
    ].
%%-------------------------------------------------------------------
%% Operator get db status test
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    Cmd = {get_db_stats, {ok, SessId, []}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, List} = Result,

    ?assertEqual(true, is_list(List)),
    {ok, Json} = hd(List),

    ?assertEqual(true, is_list(Json)),
    ?assertEqual({get_db_stats, success}, CmdRes),
    ?debugMsg("GET DB STATS TEST SUCCESS finished").

invalid(_Callback, _SessId) ->
    ok.
