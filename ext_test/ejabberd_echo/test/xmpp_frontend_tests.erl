%%%==================================================================
%%% @doc
%%% Integration tests of the Treacherous Talks game system via
%%%  the xmpp_frontend application using exmpp as an xmpp client.
%%%
%%% This assumes the xmpp_frontend is running a server on the
%%% hostname 'localhost' and is connected to a backend release.
%%%
%%% The time waited for the response to be received
%%% is defined in RESPONSE_WAIT_PERIOD.
%%%
%%%==================================================================

-module(xmpp_frontend_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Some macros which define predefined messages and value for running
%% these tests.
%%-------------------------------------------------------------------
-define(RESPONSE_WAIT_PERIOD, 500).
-define(SERVICE_BOT, "service@tt.localhost").


%% ------------------------------------------------------------------
%% Requests
%%-------------------------------------------------------------------
-define(ECHO_TEST_MSG, "Echo test message").
-define(REGISTER_COMMAND_CORRECT,
"REGISTER
NICKNAME: nickname
PASSWORD: pass
FULLNAME: Full Name
EMAIL: sth@sth
END").
-define(LOGIN_COMMAND_CORRECT,
"LOGIN
NICKNAME: nickname
PASSWORD: pass
END").
-define(LOGIN_COMMAND_BAD_PASSWORD,
"LOGIN
NICKNAME: nickname
PASSWORD: wrongpwd
END").
-define(LOGIN_COMMAND_BAD_NICK,
"LOGIN
NICKNAME: nonexistentuser
PASSWORD: pass
END").
-define(UPDATE_COMMAND_CORRECT,
"UPDATE
NICKNAME: nickname
PASSWORD: pass
FULLNAME: Full Nameeee
EMAIL: sth@sth
END").
-define(UPDATE_COMMAND_MISSING_FIELD,
"UPDATE
PASSWORD: pass
FULLNAME: Full Nameeee
EMAIL: ath@sth
END").
-define(CREATE_COMMAND_CORRECT,
"CREATE
GAMENAME: bob
PRESSTYPE: none
ORDERCIRCLE: 1D
RETREATCIRCLE: 1D
GAINLOSTCIRCLE: 1D
WAITTIME: 1D
END").
-define(CREATE_COMMAND_MISSING_FIELDS,
"CREATE
ORDERCIRCLE: 1
RETREATCIRCLE: 1
GAINLOSTCIRCLE: 1
WAITTIME: 1
END").


%%------------------------------------------------------------------
%% Responses
%%------------------------------------------------------------------
-define(REG_RESPONSE_SUCCESS, "Registration was successful.").
-define(REG_RESPONSE_BAD_SYNTAX,
"The command [register] could not be interpreted correctly:
Required fields: [\"NICKNAME\",\"PASSWORD\",\"EMAIL\",\"FULLNAME\"]").
-define(LOGIN_RESPONSE_SUCCESS, "Login was successful. Your session is:").
-define(UPD_RESPONSE_SUCCESS,
        "User information was successfully updated.").
-define(UPD_RESPONSE_BAD_SYNTAX, "Invalid user update information.").
-define(CREATE_RESPONSE_SUCCESS, "Game creation was successful.").
-define(CREATE_RESPONSE_BAD_SYNTAX, "The command [create_game] could not be interpreted correctly:
Required fields: [\"GAMENAME\",\"PRESSTYPE\",\"ORDERCIRCLE\",\"RETREATCIRCLE\",
                  \"GAINLOSTCIRCLE\",\"WAITTIME\"]").
-define(RESPONSE_COMMAND_UNKNOWN, "The provided command is unknown.
Supported commands are:").


%%-------------------------------------------------------------------
%% @doc
%%-------------------------------------------------------------------
basic_fixture_test_ () ->
    {setup,
     fun setup_basic/0,
     fun teardown/1,
     [fun ejabberd_echo/0,
      fun xmpp_empty_message/0]}.

instantiator_fixture_test_() ->
    {setup,
     fun setup_for_instantiator/0,
     fun teardown/1,
     fun instantiator/1}.

%%-------------------------------------------------------------------
%% @doc
%%  Setup to prepare for running tests.
%%
%%  Start exmpp client, make session and do handshaking with
%%  ejabberd server.
%%  You can pass parameter to start_link function to
%%  make session with your user and configure.
%%  For more infomation, see xmpp_client:startlink/3.
%%-------------------------------------------------------------------
setup_basic() ->
    ?debugMsg("Setting up xmpp tests. "),
    xmpp_client:start_link(),
    ok.

setup_for_instantiator() ->
    ?debugMsg("Setting up xmpp tests. "),
    xmpp_client:start_link(),
    [{?REGISTER_COMMAND_CORRECT, ?REG_RESPONSE_SUCCESS, "valid registration attempt"}
%     {"REGISTER fdg END", ?REG_RESPONSE_BAD_SYNTAX, "register command with missing fields"},
%     {"register me", ?RESPONSE_COMMAND_UNKNOWN, "request matching no command"},
%     {?LOGIN_COMMAND_CORRECT, ?LOGIN_RESPONSE_SUCCESS, "valid login attempt"},
%     {?LOGIN_COMMAND_BAD_PASSWORD, "Invalid login data.", "login request with invalid password"},
%     {?LOGIN_COMMAND_BAD_NICK, "Invalid login data.","login request with invalid nickname"},
%     {?UPDATE_COMMAND_CORRECT, ?UPD_RESPONSE_SUCCESS, "valid update attempt"},
%     {?UPDATE_COMMAND_MISSING_FIELD, ?UPD_RESPONSE_BAD_SYNTAX, "update command with missing fields"},
%     {?CREATE_COMMAND_CORRECT, ?CREATE_RESPONSE_SUCCESS, "valid create command"},
%     {?CREATE_COMMAND_MISSING_FIELDS, ?CREATE_RESPONSE_BAD_SYNTAX, "create command with missing fields"}
    ].


%%-------------------------------------------------------------------
%% @doc
%%
%%  Clean up environment after runnning tests.
%%
%%  stop exmpp client and close the session
%%-------------------------------------------------------------------
teardown(_)->
    ?debugMsg("Tearing down xmpp test."),
    % stop spewing output to tty before we kill xmpp_client
    % because it's noisy.
    error_logger:tty(false),
    xmpp_client:stop().


%%-------------------------------------------------------------------
%% instantiate common tests
%%-------------------------------------------------------------------
instantiator([{Request, Expect, Description}|Rest]) ->
    Test = fun() ->
                   ?debugFmt("Testing ~s.", [Description]),
                   %?debugFmt("Sending \"~s\" to ~s.~n",[Request, ?SERVICE_BOT]),
                   ok = xmpp_client:clear_last_received(),
                   xmpp_client:send_message(?SERVICE_BOT, Request),
                   timer:sleep(?RESPONSE_WAIT_PERIOD),
                   Response = xmpp_client:last_received_msg(),
                   ResponsePrefix = response_prefix(Expect, Response),
                   ?assertEqual(Expect, ResponsePrefix)
           end,
    [Test | instantiator(Rest)];
instantiator([]) ->
    [].


%%-------------------------------------------------------------------
%% @doc
%%  run echo test with ejabberd server. it send a message to a user at
%%  echo local host and get the same message
%%-------------------------------------------------------------------
ejabberd_echo() ->
    ok = xmpp_client:clear_last_received(),
    xmpp_client:send_message("tester@echo.localhost", ?ECHO_TEST_MSG),
    timer:sleep(?RESPONSE_WAIT_PERIOD),
    RMsg = xmpp_client:last_received_msg(),
    ?assertEqual(?ECHO_TEST_MSG, RMsg).


%%-------------------------------------------------------------------
%% @doc
%%  Send an empty message to service component.
%%  Expect it to be ignored since it will contain no xml body element.
%%-------------------------------------------------------------------
xmpp_empty_message() ->
    ok = xmpp_client:clear_last_received(),
    xmpp_client:send_message(?SERVICE_BOT, ""),
    timer:sleep(?RESPONSE_WAIT_PERIOD),
    Response = xmpp_client:last_received_msg(),
    ?assertEqual(undefined, Response).


%%------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% @doc Return the prefix of the actual value shortened to the length
%% of the expected value.
%%
%% This is for cases where the first part of the response is all that
%% is needed for deciding if things worked as expected, and the rest
%% is variable, e.g. while we pass back an entire user record.
%%
%% Details like whether the details passed to the controller to register
%% were all saved correctly are the responsibility of the controller
%% integration tests.
%%
%% NOTE: This has the risk of making tests pass if you make the expected
%% value the empty string, hence the clause for empty Expected.
%%------------------------------------------------------------------
response_prefix("", _Actual) ->
    "misuse of response_prefix/2";
response_prefix(Expected, Actual) ->
    lists:sublist(Actual, length(Expected)).

