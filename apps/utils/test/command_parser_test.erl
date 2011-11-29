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
-module(command_parser_test).
-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}
-include_lib("datatypes/include/message.hrl").
-include("include/command_parser.hrl").% User command keyword


-include("test_utils.hrl").

parse_test_() ->
    [
     ?_test(check_parse(?SAMPLE_REGISTER,
                        {register, {ok, #user{nick = "Lin", password = "QWER",
                                              email = "ss@lin.pcs",
                                              channel = im,
                                              name = "Agner Erlang"}}
                        })),
     ?_test(check_parse(?SAMPLE_UPDATE,
                        {update_user, {ok, ?SESSION_ID,
                                       [{#user.password,"QWER"},
                                        {#user.email,field_missing},
                                        {#user.name,"Agner Erlang"}]}
                        })),
     ?_test(check_parse(?SAMPLE_CREATE,
                        {create_game, {ok, ?SESSION_ID,
                                       #game{name = "awesome_game", press = white,
                                             order_phase = 240, retreat_phase = 210,
                                             build_phase = 160, waiting_time = 3200,
                                             description = "",
                                             password = "1234",
                                             num_players = 0,
                                             creator_id = undefined}}
                        })),
     ?_test(check_parse(?SAMPLE_LOGIN,
                        {login, {ok, #user{nick = "Lin", password = "QWER"}}})),
     ?_test(check_parse(?SAMPLE_GAME_OVERVIEW,
                        {game_overview, {ok, ?SESSION_ID, 111222}})),
     ?_test(check_parse(?SAMPLE_JOIN_GAME,
                        {join_game, {ok, ?SESSION_ID, {111222, england}}})),
     ?_test(check_parse(?SAMPLE_GET_PROFILE(?SESSION_ID),
                        {get_session_user, {ok, ?SESSION_ID, no_arg}}))
    ].

check_parse(Sample, Expected) ->
    %io:format(user, "val=~p~n Exp: ~p~n", [command_parser:parse(Sample), Expected]),
    %?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample, im)).


reconfig_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_RECONFIG, im),
    Expected = {reconfig_game,{ok,?SESSION_ID, {
                                    111222,
                                    [{4,"awesome_game"},    % name
                                     {7,white},
                                     {8,240},
                                     {9,210},
                                     {10,160},
                                     {14,3200},
                                     {5,field_missing},     % description
                                     {11,field_missing},    % num_players
                                     {12,"1234"},
                                     {3,field_missing}]}}}, % creator_id
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

order_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_TEST_ORDERS2, test),
    %io:format(user, "~p~n", [ActualOutput]),
    Expected = {game_order,{ok,"g2dkABFiYWNrZW5kQDEyNy4wLjAuMQAAA+QAAAAAAQ==",
                {3958230945903,
                 [{convoy,fleet,north_sea,army,london,norwegian_sea},
                  {convoy,fleet,north_sea,army,london,norwegian_sea},
                  {move,army,london,norwegian_sea,any_coast},
                  {move,army,london,norwegian_sea,north_coast},
                  {move,army,london,norwegian_sea,any_coast},
                  {move,army,london,norwegian_sea,any_coast}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

user_msg_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_USER_MSG(?SESSION_ID)
                                        , im),
    Expected = {user_msg,
                   {ok,?SESSION_ID,
                       {frontend_msg,"nick",
                           "\n\n    A sample message to nick player which\n"
                       "    contain several line\n    have fun\n\n    ", undefined}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

wrong_user_msg_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_USER_MSG_WRONG, im),
    Expected = {user_msg,{error, {required_fields,
                                  [?CONTENT, ?SESSION, ?TO]}}},

    [
        ?_assertEqual(Expected, ActualOutput)
    ].

wrong_session_user_msg_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_USER_MSG("@1234")
                                        , im),
    Expected = {user_msg,{error, {invalid_input,
                                  [?SESSION]}}},

    [
        ?_assertEqual(Expected, ActualOutput)
    ].

power_user_msg_test_() ->
    fun()->
            ?debugMsg("correct power message parsing"),
            ActualOutput = command_parser:parse(
                             ?SAMPLE_POWER_MSG(?SESSION_ID, ?GAME_ID_VAL), im),
            Expected = {power_msg,
                        {ok,?SESSION_ID,
                         {frontend_msg,[england],
                          "\n\n    A sample message to nick player which\n"
                          "    contain several line\n    have fun\n\n    ",
                          list_to_integer(?GAME_ID_VAL)}}},
            ?assertEqual(Expected, ActualOutput)
    end.

game_msg_test_() ->
    [
     fun() ->
              ?debugMsg("game message parsing with missed game id"),
             ActualOutput = command_parser:parse(
                              ?SAMPLE_GAME_MSG(?SESSION_ID, "dfgdf"), im),
             Expected = {game_msg,{error, {invalid_input,
                                  [?GAMEID]}}},

             ?assertEqual(Expected, ActualOutput)
     end,
     fun() ->
             ?debugMsg("correct game message parsing"),
             ActualOutput = command_parser:parse(
                              ?SAMPLE_GAME_MSG(?SESSION_ID, ?GAME_ID_VAL), im),
             Expected = {game_msg,
                         {ok,?SESSION_ID,
                          {frontend_msg,[england],
                           "\n\n    A sample message to nick player which\n"
                               "    contain several line\n    have fun\n\n    ",
                            list_to_integer(?GAME_ID_VAL)}}},

             ?assertEqual(Expected, ActualOutput)
     end,
     fun() ->
             ?debugMsg("game message field missing"),
             ActualOutput = command_parser:parse(
                          ?SAMPLE_GAME_MSG_WRONG(?SESSION_ID, ?GAME_ID_VAL), im),
             Expected = {game_msg,{error, {required_fields,
                                  [?CONTENT, ?SESSION, ?GAMEID]}}},

             ?assertEqual(Expected, ActualOutput)
     end,
     fun() ->
             ?debugMsg("correct game message parsing(send to 4 countries)"),
             ActualOutput = command_parser:parse(
                              ?SAMPLE_GAME_MSG_MULTICOUNTRY(?SESSION_ID,
                                                            ?GAME_ID_VAL), im),
             Expected = {game_msg,
                         {ok,?SESSION_ID,
                          {frontend_msg,[austria,russia,england,germany],
                           "\n\n    A sample message to nick player which\n"
                               "    contain several line\n    have fun\n\n    ",
                            list_to_integer(?GAME_ID_VAL)}}},

             ?assertEqual(Expected, ActualOutput)
     end,
     fun() ->
             ?debugMsg("implicitly broadcast to all countries"),
             ActualOutput = command_parser:parse(
                              ?SAMPLE_GAME_MSG_NO_TO(?SESSION_ID,
                                                            ?GAME_ID_VAL), im),
             Expected = {game_msg,
                         {ok,?SESSION_ID,
                          {frontend_msg,[austria, england, france, germany,
                                         italy, russia, turkey],
                           "\n\n    A sample message to nick player which\n"
                               "    contain several line\n    have fun\n\n    ",
                            list_to_integer(?GAME_ID_VAL)}}},

             ?assertEqual(Expected, ActualOutput)
     end
    ].

unit_missing_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_TEST_ORDERS_UNIT_MISSING, im),
    %io:format(user, "~p~n", [ActualOutput]),
    Expected = {game_order,
    {error,
        {invalid_input,
            [{error,{"invalid action#",[[],"Lon","-",[],"Nrg",[],[]]}}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

action_missing_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_TEST_ORDERS_ACTION_MISSING, im),
    %io:format(user, "~p~n", [ActualOutput]),
    Expected = {game_order,
                   {error,
                       {invalid_input,
                           [{error,"    A Nrg#bad order format"}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].
