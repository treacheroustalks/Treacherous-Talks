%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for getting games.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([success/3, invalid/3]).
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    Cmd = {get_game, {ok, SessId, GameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({get_game, success}, CmdRes),

    Game = ResultData,
    ?assertEqual(Game#game.id, GameId).

invalid(Callback, SessId, _GameId) ->
    FakeGameId = get_test_data(invalid),
    Cmd = {get_game, {ok, SessId, FakeGameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({get_game, invalid_data}, CmdRes),
    ?assertEqual(game_does_not_exist, ResultData),
    ok.
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(invalid) ->
    0.