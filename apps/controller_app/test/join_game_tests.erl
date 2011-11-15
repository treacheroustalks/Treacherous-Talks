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
%%% @doc Unit tests for joining games games.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(join_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([tests/3, success/3, invalid/3]).

tests(Callback, SessId, GameId) ->
    [
     ?_test(success(Callback, SessId, GameId)),
     ?_test(invalid(Callback, SessId, GameId))
    ].
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    Cmd = {join_game, {ok, SessId, {GameId, germany}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({join_game, success}, CmdRes),

    ?assertEqual(GameId, ResultData).

invalid(Callback, SessId, GameId) ->
    % same as success, but since we joined germany already
    % we can't do it again....
    % @todo add check if person is already in the game somewhere
    Cmd = {join_game, {ok, SessId, {GameId, germany}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({join_game, invalid_data}, CmdRes),

    ?assertEqual(user_already_joined, ResultData).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
