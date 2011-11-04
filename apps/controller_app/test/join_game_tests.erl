%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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

-export([success/3, invalid/3]).
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

    ?assertEqual(country_not_available, ResultData).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
