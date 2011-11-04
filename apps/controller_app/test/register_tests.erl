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

-export([success/1, invalid/1]).
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

    % @todo if checks for nickname duplicates exist remove the meck stuff
    meck:new(user_management, [passthrough]),
    meck:expect(user_management, create, 1, {error, nick_exists_already}),

    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, Info} = Result,
    ?assertEqual({register, invalid_data}, CmdRes),
    ?assertEqual(nick_exists_already, Info),

    meck:unload(user_management).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    controller_tests:create_user();
get_test_data(invalid) ->
    controller_tests:create_user().
