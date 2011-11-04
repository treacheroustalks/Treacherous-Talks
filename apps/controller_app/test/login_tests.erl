%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for login
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(login_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([success/1, invalid/1]).
%%-------------------------------------------------------------------
%% Login tests
%%-------------------------------------------------------------------
success(Callback) ->
    Cmd = {login, {ok, get_test_data(success)}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _SessionId} = Result,

    ?assertEqual({login, success}, CmdRes).

invalid(Callback) ->
    User = get_test_data(invalid),
    Cmd = {login, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({login, invalid_data}, CmdRes),
    ?assertEqual(invalid_login_data, Info).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    User = controller_tests:create_user(),
    Register = {register, {ok, User}},
    controller:handle_action(Register,
                             {fun(_,_,Data) -> Data end, []});
get_test_data(invalid) ->
    #user{nick = "u9hvawebf802bv82 RANDOM",
          password = "jksbnaugh29 RANDOM"}.
