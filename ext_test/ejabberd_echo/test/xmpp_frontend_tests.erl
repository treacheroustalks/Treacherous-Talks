%%%==================================================================
%%% @doc
%%% Integration tests of the Treacherous Talks game system via
%%%  the xmpp_frontend application using exmpp as an xmpp client.
%%%
%%% This assumes the xmpp_frontend is running a server on the
%%% hostname 'localhost' and is connected to a backend release.
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
-define(SERVICE_BOT, "service@tt.localhost").


%% ------------------------------------------------------------------
%% Requests
%%-------------------------------------------------------------------
-define(ECHO_TEST_MSG, "Echo test message").
-define(REGISTER_COMMAND_CORRECT(Nick),
"REGISTER
NICKNAME: " ++ Nick ++ "
PASSWORD: pass
FULLNAME: Full Name
EMAIL: sth@sth
END").
-define(LOGIN_COMMAND_CORRECT(Nick),
"LOGIN
NICKNAME: " ++ Nick ++ "
PASSWORD: pass
END").
-define(LOGIN_COMMAND_BAD_PASSWORD(Nick),
"LOGIN
NICKNAME: " ++ Nick ++ "
PASSWORD: wrongpwd
END").
-define(LOGIN_COMMAND_BAD_NICK,
"LOGIN
NICKNAME: nonexistentuser
PASSWORD: pass
END").
-define(UPDATE_COMMAND_CORRECT(Session),
"UPDATE
SESSION: " ++ Session ++ "
PASSWORD: pass
FULLNAME: Full Nameeee
EMAIL: sth@sth
END").
-define(UPDATE_COMMAND_MISSING_FIELD(Session),
"UPDATE
PASSWORD: pass
FULLNAME: Full Nameeee
EMAIL: ath@sth
END").
-define(CREATE_COMMAND_CORRECT(Session),
"CREATE
SESSION: " ++ Session ++ "
GAMENAME: bob
PRESSTYPE: none
ORDERCIRCLE: 1D
RETREATCIRCLE: 1D
GAINLOSTCIRCLE: 1D
WAITTIME: 1D
END").
-define(CREATE_COMMAND_MISSING_FIELDS(Session),
"CREATE
ORDERCIRCLE: 1
RETREATCIRCLE: 1
GAINLOSTCIRCLE: 1
WAITTIME: 1
END").
-define(RECONFIG_COMMAND(Session, GameID),
"RECONFIG
SESSION: " ++ Session ++ "
GAMEID: " ++ GameID ++ "
PRESSTYPE: none
ORDERCIRCLE: 2D
END").
-define(JOIN_GAME_COMMAND(Session, GameID),
"JOIN
SESSION: " ++ Session ++ "
GAMEID: " ++ GameID ++ "
COUNTRY: england
END").
-define(GAME_OVERVIEW_COMMAND(Session, GameID),
"OVERVIEW
SESSION: " ++ Session ++ "
GAMEID: " ++ GameID ++ "
END").

-define(GAME_ORDER_COMMAND(Session, GameID),
"sddfaadfaff

ORDER

SESSION: " ++ Session ++ "
GAMEID: " ++ GameID ++ "

A Lon-Nrg
Lon-Nrg
A Lon -> Nrg nc
Army Lon move Nrg

END
adfadfasdfaldfad").

-define(GAME_ORDER_COMMAND_WRONG(Session, GameID),
"sddfaadfaff

ORDER

SESSION: " ++ Session ++ "
GAMEID: " ++ GameID ++ "

A Nrg
Lon-Nrg
A Lon -> Nrg nc
Army Lon move Nrg

END
adfadfasdfaldfad").

%%------------------------------------------------------------------
%% Responses
%%------------------------------------------------------------------
-define(REG_RESPONSE_SUCCESS, "Registration was successful.").
-define(REG_RESPONSE_BAD_SYNTAX,
"The command [register] could not be interpreted correctly:
Required fields: [\"NICKNAME\",\"PASSWORD\",\"EMAIL\",\"FULLNAME\"]").
-define(LOGIN_RESPONSE_SUCCESS, "Login was successful. Your session is:").
-define(UPD_RESPONSE_SUCCESS,
        "User information was successfully updated.\n").
-define(UPD_RESPONSE_SESSION_ERROR, "Invalid user session. Please log in to continue.\n").
-define(UPD_RESPONSE_BAD_SYNTAX, "The command [update_user] could not be interpreted correctly:
Required fields: [\"SESSION\"]").
-define(CREATE_RESPONSE_SUCCESS, "Game creation was successful. Your game ID is:").
-define(CREATE_RESPONSE_SESSION_ERROR, "Invalid user session. Please log in to continue.\n").
-define(CREATE_RESPONSE_BAD_SYNTAX, "The command [create_game] could not be interpreted correctly:
Required fields: [\"SESSION\",\"GAMENAME\",\"PRESSTYPE\",\"ORDERCIRCLE\",
                  \"RETREATCIRCLE\",\"GAINLOSTCIRCLE\",\"WAITTIME\"]").
-define(RESPONSE_COMMAND_UNKNOWN, "The provided command is unknown.
Supported commands are:").
-define(RECONFIG_RESPONSE_SUCCESS, "Game information was successfully updated.\n").
-define(RECONFIG_RESPONSE_INVALID_DATA,"The game you are trying to reconfigure does not exist.\n").
-define(JOIN_GAME_RESPONSE_SUCCESS,"Join game was successful.\n").
-define(JOIN_GAME_RESPONSE_INVALID_DATA,"You have already joined this game.\n").

-define(GAME_OVERVIEW_RESPONSE_SUCCESS,"Game Overview:\n\n").
-define(GAME_OVERVIEW_RESPONSE_NOT_PLAY,"Only game players can view the game overview.\n").

-define(GAME_ORDER_RESPONSE_SUCCESS, "Game order sent successfully.\n").
-define(GAME_ORDER_RESPONSE_INVALID_INPUT, "Invalid input for the given command.\n").
-define(GAME_ORDER_RESPONSE_INVALID_DATA,"You cannot send orders to a game you are not playing.\n").
%%-------------------------------------------------------------------
%% @doc
%%-------------------------------------------------------------------
basic_fixture_test_ () ->
    {setup,
     fun setup_basic/0,
     fun teardown/1,
     [fun echo/0]}.

instantiator_fixture_reg_login_test_() ->
    {setup,
     fun setup_reg_login_instantiator/0,
     fun teardown/1,
     fun reg_login_instantiator/1}.

instantiator_fixture_test_() ->
    {setup,
     fun setup_session_instantiator/0,
     fun teardown/1,
     fun session_instantiator/1}.

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

setup_reg_login_instantiator() ->
    ?debugMsg("Setting up xmpp tests. "),
    xmpp_client:start_link(),
    Nick = random_nick(),
    [{?REGISTER_COMMAND_CORRECT(Nick),
      ?REG_RESPONSE_SUCCESS,
      "valid registration attempt"},
     {"REGISTER fdg END",
      ?REG_RESPONSE_BAD_SYNTAX,
      "register command with missing fields"},
     {"register me",
      ?RESPONSE_COMMAND_UNKNOWN,
      "request matching no command"},
     {?LOGIN_COMMAND_CORRECT(Nick),
      ?LOGIN_RESPONSE_SUCCESS,
      "valid login attempt"},
     {?LOGIN_COMMAND_BAD_PASSWORD(Nick),
      "Invalid login data.\n",
      "login request with invalid password"},
     {?LOGIN_COMMAND_BAD_NICK,
      "Invalid login data.\n",
      "login request with invalid nickname"}].

setup_session_instantiator() ->
    xmpp_client:start_link(),
    Nick = random_nick(),
    xmpp_client:xmpp_call(?SERVICE_BOT, ?REGISTER_COMMAND_CORRECT(Nick)),
    Response = xmpp_client:xmpp_call(?SERVICE_BOT, ?LOGIN_COMMAND_CORRECT(Nick)),
    {match, [Session]} = re:run(Response,
                                ?LOGIN_RESPONSE_SUCCESS ++ ".*\"\"(.*)\"\"",
                                [{capture, all_but_first, list}]),
    ?debugVal(Session),
    InvalidSession = "dGVzdA==",

    Game = xmpp_client:xmpp_call(?SERVICE_BOT, ?CREATE_COMMAND_CORRECT(Session)),
    ?debugVal(Game),
    {match, [GameID]} = re:run(Game,
                                ?CREATE_RESPONSE_SUCCESS ++ ".*\"(.*)\"",
                                [{capture, all_but_first, list}]),
    ?debugVal(GameID),
    [

     {?GAME_ORDER_COMMAND(Session, GameID),
       ?GAME_ORDER_RESPONSE_INVALID_DATA,
      "game order invalid data"},
     {?GAME_ORDER_COMMAND_WRONG(Session, GameID),
       ?GAME_ORDER_RESPONSE_INVALID_INPUT,
       "game order invalid input"},


     {?UPDATE_COMMAND_CORRECT(Session),
      ?UPD_RESPONSE_SUCCESS,
      "valid update attempt"},
     {?UPDATE_COMMAND_CORRECT(InvalidSession),
      ?UPD_RESPONSE_SESSION_ERROR,
      "update attempt with invalid session"},
     {?UPDATE_COMMAND_MISSING_FIELD(Session),
      ?UPD_RESPONSE_BAD_SYNTAX,
      "update command with missing fields"},
     {?CREATE_COMMAND_CORRECT(Session),
      ?CREATE_RESPONSE_SUCCESS,
      "valid create command"},
     {?CREATE_COMMAND_CORRECT(InvalidSession),
      ?CREATE_RESPONSE_SESSION_ERROR,
      "create attempt with invalid session"},
    {?CREATE_COMMAND_MISSING_FIELDS(Session),
      ?CREATE_RESPONSE_BAD_SYNTAX,
      "create command with missing fields"},

    {?RECONFIG_COMMAND(Session, GameID),
      ?RECONFIG_RESPONSE_SUCCESS,
      "successful reconfig game"},
    {?RECONFIG_COMMAND(Session, "1111222"),
      ?RECONFIG_RESPONSE_INVALID_DATA,
      "invalid reconfig game data"},

    {?GAME_OVERVIEW_COMMAND(Session, GameID),
      ?GAME_OVERVIEW_RESPONSE_NOT_PLAY,
      "game overview not play this game"},

    {?JOIN_GAME_COMMAND(Session, GameID),
      ?JOIN_GAME_RESPONSE_SUCCESS,
      "successful join game"},
    {?JOIN_GAME_COMMAND(Session, GameID),
      ?JOIN_GAME_RESPONSE_INVALID_DATA,
      "invalid join game data"},
    {?GAME_OVERVIEW_COMMAND(Session, GameID),
      ?GAME_OVERVIEW_RESPONSE_SUCCESS,
      "successful game overview"},

    {?GAME_ORDER_COMMAND(Session, GameID),
       ?GAME_ORDER_RESPONSE_SUCCESS,
      "game order successfully sent"}

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
%% instantiate register/login tests
%%-------------------------------------------------------------------
reg_login_instantiator([{Request, Expect, Description}|Rest]) ->
    Test = fun() ->
                   ?debugFmt("Testing ~s.", [Description]),
                   %?debugFmt("Sending \"~s\" to ~s.~n",[Request, ?SERVICE_BOT]),
                   Response = xmpp_client:xmpp_call(?SERVICE_BOT, Request),
                   ResponsePrefix = response_prefix(Expect, Response),
                   ?assertEqual(Expect, ResponsePrefix)
           end,
    [Test | reg_login_instantiator(Rest)];
reg_login_instantiator([]) ->
    [].

%%-------------------------------------------------------------------
%% instantiate tests that require session
%%-------------------------------------------------------------------
session_instantiator(In) ->
    reg_login_instantiator(In).


%%-------------------------------------------------------------------
%% @doc
%%  run echo test with ejabberd server. it send a message to a user at
%%  echo local host and get the same message
%%-------------------------------------------------------------------
echo() ->
    ok = xmpp_client:clear_last_received(),
    RMsg = xmpp_client:xmpp_call("tester@echo.localhost", ?ECHO_TEST_MSG),
    ?assertEqual(?ECHO_TEST_MSG, RMsg).


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

random_nick() ->
    "testNick" ++ integer_to_list(random_id()).

random_id() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]).
