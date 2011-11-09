%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for the registration.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(register_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([tests/1, success/1, invalid/1]).

tests(Callback) ->
    [
     ?_test(success(Callback)),
     ?_test(invalid(Callback))
    ].
%%-------------------------------------------------------------------
%% Register tests
%%-------------------------------------------------------------------
success(Callback) ->
    User = get_test_data(success),
    Cmd = {register, {ok, User}},

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, NewUser} = Result,
    ?assertEqual({register, success}, CmdRes),
    ?assertEqual(User#user{id = NewUser#user.id}, NewUser).

invalid(Callback) ->
    User = get_test_data(invalid),
    Cmd = {register, {ok, User}},

    {{register, success}, _User} = controller:handle_action(Cmd, Callback),
    % register again with that nick has to fail
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({register, invalid_data}, CmdRes),
    ?assertEqual(nick_already_exists, Info).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    controller_tests:create_user();
get_test_data(invalid) ->
    controller_tests:create_user().
