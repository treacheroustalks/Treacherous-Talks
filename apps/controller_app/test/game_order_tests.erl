%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
    Data = game_order_sample(success),
    Cmd = {game_order, {ok, SessId, {GameId, Data}}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,

    ?assertEqual({game_order,success}, CmdRes).

invalid(_Callback, _SessId, _GameId) ->
    % don't know how this could fail ...
    ok.
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
game_order_sample(success) ->
    [{move,fleet,mid_atlantic_ocean,north_atlantic_ocean,north_coast},
  %     {waive},
       {build,fleet,munich,north_coast},
       {remove,army,munich},
  %     {disband,army,munich},
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
