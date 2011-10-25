-module(ejabberd_echo_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%-------------------------------------------------------------------
%% @doc
%% some macro which define predefined message and value for runing test
%%-------------------------------------------------------------------
-define(WAIT_FOR_RESPONSE, 500).
-define(REG_RECIPIENT, "register@service.localhost").


-define(ECHO_TEST_MSG, "Echo test message").
-define(WRONG_MSG_STYLE, "Please send registration request as below it start
 with REGISTER and finish with END and enter keywords with capital letters").
-define(MSG_STYLE, "
REGISTER
NICKNAME: yournick
PASSWORD: pass
FULLNAME: fullname
EMAIL: sth@lin.sth
END").
-define(UNHANDLED_MSG, "Unhandled parse result.").
-define(SUCCESSFUL_REGISTR_MSG, "Welcome, You are succesfully registered").
-define(EMPTY_MSG_ERROR, "Please do not send empty Message!").
-define(REGISTRATION_RESULT, "Registration result: {user,").

%%-------------------------------------------------------------------
%% @doc
%%-------------------------------------------------------------------
ejabberd_test_ () ->
    {setup,
     fun xmpp_start_client/0,
     fun xmpp_stop_client/1,
     [fun ejabberd_echo/0,
      fun xmpp_register_successful_user/0,
      fun xmpp_register_empty_user/0,
      fun xmpp_register_user_wrong_msg_style/0,
      fun xmpp_register_user_wrong_msg/0]
     }.



%%-------------------------------------------------------------------
%% @doc
%%  start exmpp client make make session and do handshaking with
%%  ejabberd server. you can pass parameter to start_link function to
%%  make session with your user and configure. For more infomation
%%   see xmpp_client:startlink/3.
%%-------------------------------------------------------------------
xmpp_start_client() ->
    ?debugMsg("start xmpp test .. "),
    xmpp_client:start_link(),
    ok.

%%-------------------------------------------------------------------
%% @doc
%%  run echo test with ejabberd server. it send a message to a user at
%%  echo local host and get the same message
%%-------------------------------------------------------------------
ejabberd_echo() ->
    xmpp_client:send_message("tester@echo.localhost", ?ECHO_TEST_MSG),
    timer:sleep(?WAIT_FOR_RESPONSE),
    RMsg = xmpp_client:last_received_msg(),
    ?debugMsg(["send:",?ECHO_TEST_MSG, " receive: ",RMsg]),
    ?assertEqual(RMsg, ?ECHO_TEST_MSG).

%%-------------------------------------------------------------------
%% @doc
%%  register user with valid data and recieve a confirm for registration
%%-------------------------------------------------------------------
xmpp_register_successful_user() ->
    xmpp_client:send_message(?REG_RECIPIENT, ?MSG_STYLE),
    timer:sleep(?WAIT_FOR_RESPONSE),
    RMsg = xmpp_client:last_received_msg(),
    ?debugMsg(["send: ",?MSG_STYLE, " Receive: ",RMsg]),
    ?assertEqual(?REGISTRATION_RESULT,
                 lists:sublist(RMsg, length(?REGISTRATION_RESULT))).

%%-------------------------------------------------------------------
%% @doc
%%  send an empty message to registration component and get the corresponding
%%  response back.
%%-------------------------------------------------------------------
xmpp_register_empty_user() ->
    xmpp_client:send_message(?REG_RECIPIENT, ""),
    timer:sleep(?WAIT_FOR_RESPONSE),
    RMsg = xmpp_client:last_received_msg(),
    ?debugMsg(["send: empty message", " Receive: ",RMsg]),
    ?assertEqual(RMsg, ?EMPTY_MSG_ERROR).

%%-------------------------------------------------------------------
%% @doc
%%  send a message which is contain the registration fram but not vaild tags,
%%  for the regitration componenet
%%-------------------------------------------------------------------
xmpp_register_user_wrong_msg_style() ->
    SendMsg = "REGISTER fdg END",
    xmpp_client:send_message(?REG_RECIPIENT, SendMsg),
    timer:sleep(?WAIT_FOR_RESPONSE),
    RMsg = xmpp_client:last_received_msg(),
    ?debugMsg(["send: ",SendMsg, " Receive: ",RMsg]),
    ?assertEqual(RMsg, ?WRONG_MSG_STYLE ++ ?MSG_STYLE).

%%-------------------------------------------------------------------
%% @doc
%%  send a message which is not contain the registration fram
%%  for the regitration componenet
%%-------------------------------------------------------------------
xmpp_register_user_wrong_msg() ->
    SendMsg = "register me",
    xmpp_client:send_message(?REG_RECIPIENT, SendMsg),
    timer:sleep(?WAIT_FOR_RESPONSE),
    RMsg = xmpp_client:last_received_msg(),
    ?debugMsg(["send: ",SendMsg, " Receive: ",RMsg]),
    ?assertEqual(RMsg, ?UNHANDLED_MSG).


%%-------------------------------------------------------------------
%% @doc
%%  stop exmpp client and close the session
%%-------------------------------------------------------------------
xmpp_stop_client(_)->
    ?debugMsg("finish xmpp test"),
    xmpp_client:stop().
