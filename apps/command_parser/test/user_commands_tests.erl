%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(user_commands_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}

-include("include/records.hrl").% -record(reg_info,{})
-include("include/test_utils.hrl").% ?SAMPLE_EMAILS
-include("include/command_parser.hrl").

parse_login_test_() ->
    ActualOutput = user_commands:parse_login(?SAMPLE_LOGIN),
    Expected = {ok, #user{nick = "Lin", password = "QWER"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].


parse_register_test_() ->
    ActualOutput = user_commands:parse_register(?SAMPLE_REGISTER),
    Expected = {ok, #user{nick = "Lin", password = "QWER",
                          email = "ss@lin.pcs", name = "Agner Erlang"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].


parse_update_test_() ->
    ActualOutput = user_commands:parse_update(?SAMPLE_UPDATE),
    Expected = {ok, "Lin", [{5,"QWER"},{4,field_missing},{6,"Agner Erlang"}]},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_create_test_() ->
    ActualOutput = user_commands:parse_create(?SAMPLE_CREATE),
    Expected = {ok, #game{name = "awesome_game", press = "white",
                       order_phase = 240, retreat_phase = 210,
                       build_phase = 160, waiting_time = 3200,
                     description = field_missing,
                       password = "1234",
                        num_players = field_missing,
                       creator_id = undefined}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_time_format_test_() ->
    [
        ?_assertEqual(
            1742,
            user_commands:parse_time_format("asdfsd01D02M05Hadaf")
        )
    ].

is_valid_value_test_() ->
    [
        ?_assertEqual(
            true,
            user_commands:is_valid_value(sdfsdfs, field_missing)
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(alpha_num_only, "324asdlSDF234")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(alpha_num_only, "324asdl;SDF234")
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(begin_with_alpha, "hello234")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(begin_with_alpha, "hel%o")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(begin_with_alpha, "5helo34*")
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(num_only, "00234211")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(num_only, "asdfkl234ljsa")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(num_only, "234134%")
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(alpha_space_only, "aasdjASDFA")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(alpha_space_only, "asdfkl234ljsa")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(alpha_space_only, "ASDFjkll#ds")
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(mail_addr, "erlang@tt.pcs")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(mail_addr, "aa@aa")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(mail_addr, "assd@a*f.aa")
        ),
        ?_assertEqual(
            false,
            user_commands:is_valid_value(mail_addr, "as*sd@af.aa")
        ),

        ?_assertEqual(
            true,
            user_commands:is_valid_value(sdfsdfs, field_missing)
        )
    ].

get_error_list_test_()->
    [
     ?_assertEqual(
            [],
             user_commands:get_error_list(["lin@lin.pcs"],[mail_addr],[?EMAIL])
     ),
     ?_assertEqual(
            [?EMAIL],
             user_commands:get_error_list(["linlin.pcs"],[mail_addr],[?EMAIL])
     )

    ].

get_check_type_test_()->
    [
     ?_assertEqual(
            [mail_addr,mail_addr],
             user_commands:get_check_type([?EMAIL,?EMAIL])
     ),
     ?_assertEqual(
            [alpha_space_only,password],
             user_commands:get_check_type([?FULLNAME,?PASSWORD])
     )
     ].

parse_reconfig_test_() ->
    [
     ?_assertEqual(
            {error, {required_fields, [?GAMEID]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG_WITHOUT_GAMEID)
     ),
     ?_assertEqual(
            {error, {invalid_input, [?GAMEID]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG_WITH_CHARGID)
     ),
     ?_assertEqual(
            {ok,111222,
            [{4,"awesome_game"},
             {7,"white"},
             {8,240},
             {9,210},
             {10,160},
             {14,3200},
             {5,field_missing},
             {11,field_missing},
             {12,"1234"},
             {3,undefined}]},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG)
     )
     ].
