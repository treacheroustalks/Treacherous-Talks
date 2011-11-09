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
-module(game_overview_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").

-export([success/3, invalid/3]).
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    Cmd = {game_overview, {ok, SessId, GameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,

    ?assertEqual({game_overview, success}, CmdRes),

    ?assert(is_record(ResultData, game_overview)).

invalid(Callback, SessId, _GameId) ->
    NewGame = controller_tests:create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    NewGameId = controller:handle_action(GameCreate,
                                         {fun(_,_,Data) -> Data end, []}),

    Cmd = {game_overview, {ok, SessId, NewGameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _ResultData} = Result,

    ?assertEqual({game_overview, invalid_data}, CmdRes).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
