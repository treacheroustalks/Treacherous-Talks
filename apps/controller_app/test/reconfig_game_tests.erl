%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
    Data = get_test_data(success),
    Cmd = {reconfig_game, {ok, SessId, {GameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({reconfig_game, success}, CmdRes),

    GameId = ResultData,
    ?assert(is_integer(GameId)).

invalid(Callback, SessId, _GameId) ->
    Data = get_test_data(success),
    FakeGameId = get_test_data(invalid),
    Cmd = {reconfig_game, {ok, SessId, {FakeGameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({reconfig_game, invalid_data}, CmdRes),
    ?assertEqual(game_does_not_exist, ResultData),
    ok.
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
