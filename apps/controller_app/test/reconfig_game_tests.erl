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
%%% @doc Unit tests for reconfiguring games games.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(reconfig_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([tests/1, success/3, invalid/3]).

tests([Callback, SessId, GameId]) ->
    [
     ?_test(success(Callback, SessId, GameId)),
     ?_test(invalid(Callback, SessId, GameId))
    ].
%%-------------------------------------------------------------------
%% Reconfig game tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    ?debugMsg("RECONFIG_GAME TEST SUCCESS"),
    Data = get_test_data(success),
    Cmd = {reconfig_game, {ok, SessId, {GameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({reconfig_game, success}, CmdRes),

    GameId = ResultData,
    ?assert(is_integer(GameId)),
    ?debugMsg("RECONFIG_GAME TEST SUCCESS finished").

invalid(Callback, SessId, _GameId) ->
    ?debugMsg("RECONFIG_GAME TEST INVALID"),
    Data = get_test_data(success),
    FakeGameId = get_test_data(invalid),
    Cmd = {reconfig_game, {ok, SessId, {FakeGameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({reconfig_game, invalid_data}, CmdRes),
    ?assertEqual(game_does_not_exist, ResultData), 
    ?debugMsg("RECONFIG_GAME TEST INVALID finished").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    [{4,"awesome_game"},
     {7,"white"},
     {8,240},
     {9,210},
     {10,160},
     {14,3200},
     {5,field_missing},
     {11,field_missing},
     {12,"1234"},
     {3,undefined}];
get_test_data(invalid) ->
    0.
