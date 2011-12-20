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
%% Author: lin
%% Created: Oct 20, 2011
%% Description: TODO: Add description to user_commands
-module(user_commands_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}
-include_lib("datatypes/include/message.hrl").% #report_message{}

-include("include/test_utils.hrl").% ?SAMPLE_EMAILS
-include("include/command_parser.hrl").

parse_login_test_() ->
    ActualOutput = user_commands:parse_login(?SAMPLE_LOGIN),
    Expected = {ok, #user{nick = "Lin", password = "QWER"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_logout_test_() ->
    ActualOutput = user_commands:parse_logout(?SAMPLE_LOGOUT),
    Expected = {ok, ?SESSION_ID, no_arg},
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

parse_register_channel_test_() ->
    ActualOutput = user_commands:parse_register(?SAMPLE_REGISTER_CHANNEL),
    Expected = {ok, #user{nick = "Lin", password = "QWER", channel = mail,
                          email = "ss@lin.pcs", name = "Agner Erlang"}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_update_test_() ->
    ActualOutput = user_commands:parse_update(?SAMPLE_UPDATE),
    Expected = {ok, ?SESSION_ID, [{#user.password,"QWER"},
                                  {#user.email,field_missing},
                                  {#user.name,"Agner Erlang"}]},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_create_test_() ->
    ActualOutput = user_commands:parse_create(?SAMPLE_CREATE),
    Expected = {ok, ?SESSION_ID, #game{name = "awesome_game", press = white,
                                  order_phase = 240, retreat_phase = 210,
                                  build_phase = 160, waiting_time = 3200,
                                  description = "",
                                  password = "1234",
                                  num_players = 0,
                                  creator_id = undefined}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_game_overview_test_() ->
    ActualOutput = user_commands:parse_overview(?SAMPLE_GAME_OVERVIEW),
    Expected = {ok, ?SESSION_ID, 111222},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_games_current_test_() ->
    ActualOutput = user_commands:parse_games_current(
                                     ?SAMPLE_GAMES_CURRENT(?SESSION_ID)),
    Expected = {ok, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==",
                    useless_data},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_join_game_test_() ->
    ActualOutput = user_commands:parse_join(?SAMPLE_JOIN_GAME),
    Expected = {ok, ?SESSION_ID, {111222, england}},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_game_search_test_() ->
    ActualOutput = user_commands:parse_game_search(
                                     ?SAMPLE_GAME_SEARCH(?SESSION_ID)),
    Expected = {ok, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==",
                    "id=1234 AND name=simple_game AND waiting_time=1D"},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_join_game_error_test_() ->
    ActualOutput = user_commands:parse_join(?SAMPLE_JOIN_GAME_ERROR),
    Expected = {error, {invalid_input, "Dummyland"}},
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

parse_get_presence_test_() ->
    ActualOutput = user_commands:parse_get_presence(
                                     ?SAMPLE_GET_PRESENCE(?SESSION_ID)),
    Expected = {ok, "g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==",
                    "testusernick"},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_send_report_test_() ->
    ActualOutput = user_commands:parse_send_report(
                     ?SAMPLE_PLAYER_REPORT(?SESSION_ID), report_player),
    Expected = {ok, ?SESSION_ID, #report_message{to = moderator,
                                                 type = report_player,
                                                 content = "report a player"}},
    [
     ?_assertEqual(Expected, ActualOutput)
    ].

parse_blacklist_test_() ->
    ActualOutput = user_commands:parse_blacklist(?SAMPLE_BLACKLIST),
    Expected = {ok, ?SESSION_ID, "Lin"},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

parse_whitelist_test_() ->
    ActualOutput = user_commands:parse_whitelist(?SAMPLE_WHITELIST),
    Expected = {ok, ?SESSION_ID, "Lin"},
    [
        ?_assertEqual(Expected, ActualOutput)
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
            {error, {required_fields, [?GAMEID, ?SESSION]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG_WITHOUT_GAMEID)
     ),
     ?_assertEqual(
            {error, {invalid_input, [?SESSION, ?GAMEID]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG_WITH_CHARGID)
     ),
     ?_assertEqual(
            {ok, ?SESSION_ID,
             {111222,
              [{4,"awesome_game"},
               {7,white},
               {8,240},
               {9,210},
               {10,160},
               {14,3200},
               {5,field_missing},   % description
               {11,field_missing},  % num_players
               {12,"1234"},
               {3,field_missing}]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG)
     ),
     ?_assertEqual(
            {ok, ?SESSION_ID,
             {111222,
              [{4,"awesome_game"},
               {7,white},
               {8,240},
               {9,field_missing},
               {10,field_missing},
               {14,field_missing},
               {5,field_missing},   % description
               {11,field_missing},  % num_players
               {12,"1234"},
               {3,field_missing}]}},
             user_commands:parse_reconfig(?SAMPLE_RECONFIG_2)
     )

     ].
