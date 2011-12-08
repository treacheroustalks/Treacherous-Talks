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
%%% @doc Unit tests for stopping a game
%%% @end
%%%
%%% @since : 6 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(stop_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("utils/include/test_utils.hrl").

-export([tests/1, success/2, access_denied/1]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId)),
     ?_test(access_denied(Callback))
    ].
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    {setup,
     fun() -> %setup
             ?debugMsg("STOP GAME TEST EXPECT SUCCESS"),
             GameID = create_game(SessId),
             GameID
     end,
     fun(GameID) -> %instatiator
             fun() ->
                     Cmd = {stop_game, {ok, SessId, GameID}},
                     ?debugFmt("Before stop_game ~p~n", [?NOW_UNIV]),
                     Result = controller:handle_action(Cmd, Callback),
                     ?debugFmt("After stop_game ~p~n", [?NOW_UNIV]),
                     {CmdRes, _Info} = Result,
                     ?assertEqual({stop_game, success}, CmdRes),
                     ?debugMsg("STOP GAME TEST EXPECT SUCCESS finished")
             end
     end}.


access_denied(Callback) ->
    {setup,
     fun() -> % setup
             ?debugFmt("STOP GAME ACCESS DENIED TEST ~p~n", [?NOW_UNIV]),
             FakeOperator = get_test_operator(invalid),
             SessId = register_and_login(FakeOperator),
             SessId
     end,
     fun(SessId) -> %instantiator
             fun() ->
                     Cmd = {stop_game, {ok, SessId, 12345}},
                     ?debugFmt("Before stop_game ~p~n", [?NOW_UNIV]),
                     Result = controller:handle_action(Cmd, Callback),
                     ?debugFmt("After stop_game ~p~n", [?NOW_UNIV]),
                     {CmdRes, _Info} = Result,
                     ?assertEqual({stop_game, access_denied}, CmdRes),
                     ?debugMsg("STOP GAME ACCESS DENIED TEST finished")
             end
     end}.


%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_operator(invalid) ->
    controller_tests:create_user().

register_and_login(User) ->
    Reply = {fun(_,_,Data) -> Data end, []},
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),
    Login = {login, {ok, {NewUser, controller_tests:get_receiver()}}},
    controller:handle_action(Login, Reply).

create_game(SessId) ->
    Reply = {fun(_,_,Data) -> Data end, []},
    NewGame = controller_tests:create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    controller:handle_action(GameCreate, Reply).
