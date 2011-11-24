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
%%% @doc Unit tests for updating user
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(game_order_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/3, success/3]).

tests(Callback, SessId, GameId) ->
    [
     success(Callback, SessId, GameId)
    ].
%%-------------------------------------------------------------------
%% game order tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    ?debugMsg("GAME_ORDER TEST SUCCESS"),
    Data = game_order_sample(success),
    Cmd = {game_order, {ok, SessId, {GameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,

    ?assertEqual({game_order,success}, CmdRes),
    ?debugMsg("GAME_ORDER TEST SUCCESS finished").
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
game_order_sample(success) ->
    [{move,fleet,mid_atlantic_ocean,north_atlantic_ocean,north_coast},
       {build,fleet,munich,north_coast},
       {disband,army,munich},
       {support_move,army,munich,fleet,kiel,berlin,any_coast},
       {support_move,army,munich,fleet,kiel,berlin,any_coast},
       {support_hold,fleet,finland,any_unit,brest},
       {support_hold,fleet,finland,any_unit,brest},
       {hold,any_unit,holland},
       {hold,army,brest},
       {hold,army,brest},
       {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
       {convoy,fleet,western_mediterranean,army,brest,marseilles},
       {convoy,fleet,finland,army,brest,marseilles},
       {move,army,brest,marseilles,any_coast},
       {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
       {convoy,fleet,western_mediterranean,army,brest,marseilles},
       {convoy,fleet,finland,army,brest,marseilles},
       {move,army,brest,marseilles,any_coast},
       {convoy,fleet,north_sea,army,london,norwegian_sea},
       {convoy,fleet,north_sea,army,london,norwegian_sea},
       {move,army,london,norwegian_sea,any_coast},
       {move,army,london,norwegian_sea,north_coast},
       {move,any_unit,london,norwegian_sea,any_coast},
       {move,army,london,norwegian_sea,any_coast}];
game_order_sample(invalid) ->
    ok.
