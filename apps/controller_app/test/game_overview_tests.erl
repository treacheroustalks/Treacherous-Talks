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
-module(game_overview_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([tests/1, success/3, invalid/3]).

tests([Callback, SessId, GameId]) ->
    [
     ?_test(success(Callback, SessId, GameId)),
     ?_test(invalid(Callback, SessId, GameId))
    ].
%%-------------------------------------------------------------------
%% game overview tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    ?debugMsg("GAMES_OVERVIEW TEST SUCCESS"),
    game_timer:sync_event(GameId, timeout),
    timer:sleep(1),
    Cmd = {game_overview, {ok, SessId, GameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({game_overview, success}, CmdRes),

    ?assert(is_record(ResultData, game_overview)),
    ?debugMsg("GAMES_OVERVIEW TEST SUCCESS finished").

invalid(Callback, SessId, _GameId) ->
    ?debugMsg("GAMES_OVERVIEW TEST INVALID"),
    NewGame = controller_tests:create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    NewGameId = controller:handle_action(GameCreate,
                                         {fun(_,_,Data) -> Data end, []}),

    Cmd = {game_overview, {ok, SessId, NewGameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _ResultData} = Result,

    ?assertEqual({game_overview, invalid_data}, CmdRes),
    ?debugMsg("GAMES_OVERVIEW TEST INVALID finished").

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
