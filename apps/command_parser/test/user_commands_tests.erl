%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(user_commands_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}

-include("include/records.hrl").% -record(reg_info,{})
-include("include/test_utils.hrl").% ?SAMPLE_EMAILS

parse_login_test_() ->
    ActualOutput = user_commands:parse_login(?SAMPLE_LOGIN),
    Expected = {ok, #user{nick = "Lin", password = "QWER"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].


parse_register_test_() ->
    ActualOutput = user_commands:parse_register(?SAMPLE_REGISTER),
    Expected = {ok, #user{nick = "Lin", password = "QWER",
                          email = "ss@pcs", name = "Agner Erlang"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].


parse_update_test_() ->
    ActualOutput = user_commands:parse_update(?SAMPLE_UPDATE),
    Expected = {ok, #user{nick = "Lin", password = "QWER",
                          name = "Agner Erlang"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_create_test_() ->
    ActualOutput = user_commands:parse_create(?SAMPLE_CREATE),
    Expected = {ok, #game{name = "awesome_game", press = "white",
                       order_phase = "4H", retreat_phase = "3H30M",
                       build_phase = "2H40M", waiting_time = "2D5H20M",
                       creator_id = undefined}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].