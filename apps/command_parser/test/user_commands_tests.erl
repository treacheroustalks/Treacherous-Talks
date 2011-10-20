%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(user_commands_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include("include/records.hrl").% -record(reg_info,{})
-include("test_utils.hrl").% ?SAMPLE_EMAILS

parse_login_test_() ->
    ActualOutput = user_commands:parse_login(?SAMPLE_LOGIN),
    Expected = {ok, #user{nick = "Lin", password = "QWER"}},
    [
        ?_test(?_assertEqual(ActualOutput, Expected))
    ].


parse_register_test_() ->
    ActualOutput = user_commands:parse_register(?SAMPLE_REGISTER),
    Expected = {ok, #user{nick = "Lin", password = "QWER",
                          email = "ss@pcs", name = "Agner Erlang"}},
    [
        ?_test(?_assertEqual(ActualOutput, Expected))
    ].


parse_update_test_() ->
    ActualOutput = user_commands:parse_register(?SAMPLE_UPDATE),
    Expected = {ok, #user{nick = "Lin", password = "QWER",
                          name = "Agner Erlang"}},
    [
        ?_test(?_assertEqual(ActualOutput, Expected))
    ].