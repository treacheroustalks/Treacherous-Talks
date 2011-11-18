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

-define(SEND_OFF_GAME_MSG(Session, ToNick, Msg),
"MESSAGE
SESSION: " ++ Session ++ "
TO: " ++ ToNick ++ "
CONTENT:" ++ Msg ++ "
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

-define(GAME_OVERVIEW_RESPONSE_SUCCESS,"Unhandled error.").
-define(GAME_OVERVIEW_RESPONSE_NOT_PLAY, "Only game players can view the gam").

-define(GAME_ORDER_RESPONSE_SUCCESS, "Game order sent successfully:\n").
-define(GAME_ORDER_RESPONSE_INVALID_INPUT, "Invalid input for the given command.\n").
-define(GAME_ORDER_RESPONSE_INVALID_DATA,"You cannot send orders to a game you are not playing.\n").
-define(SEND_OFF_GAME_MSG_RESPONSE_SUCCESS, "Message was send. Message ID is: ").
-define(SEND_OFF_GAME_MSG_RESPONSE_FAILED, "Error: The user does not exist.").
%%-------------------------------------------------------------------
%% @doc
%%-------------------------------------------------------------------
basic_fixture_test_ () ->
    {setup,
     fun setup_basic/0,
     fun teardown/1,
     fun echo/1}.

instantiator_fixture_reg_login_test_() ->
    {setup,
     fun setup_reg_login_instantiator/0,
     fun teardown/1,
     fun instantiator/1}.

instantiator_fixture_test_() ->
    {setup,
     fun setup_session_instantiator/0,
     fun teardown/1,
     fun instantiator/1}.

instantiator_two_users_test_() ->
    {setup,
     fun setup_two_user_instantiator/0,
     fun teardown/1,
     fun two_user_instantiator/1}.

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
    Client = basic_test_client,
    xmpp_client:start_link(Client, "password"),
    [[Client]].

setup_reg_login_instantiator() ->
    ?debugMsg("Setting up xmpp tests. "),
    Client = reg_login_test_client,
    Password = "password",
    xmpp_client:start_link(Client, Password),
    Nick = random_nick(),
    [[Client],
     {Client, 
      ?REGISTER_COMMAND_CORRECT(Nick),
      ?REG_RESPONSE_SUCCESS,
      "valid registration attempt"},
     {Client, 
      "REGISTER fdg END",
      ?REG_RESPONSE_BAD_SYNTAX,
      "register command with missing fields"},
     {Client, 
      "register me",
      ?RESPONSE_COMMAND_UNKNOWN,
      "request matching no command"},
     {Client, 
      ?LOGIN_COMMAND_CORRECT(Nick),
      ?LOGIN_RESPONSE_SUCCESS,
      "valid login attempt"},
     {Client, 
      ?LOGIN_COMMAND_BAD_PASSWORD(Nick),
      "Invalid login data.\n",
      "login request with invalid password"},
     {Client, 
      ?LOGIN_COMMAND_BAD_NICK,
      "Invalid login data.\n",
      "login request with invalid nickname"}].

setup_session_instantiator() ->
    Client = session_test_client,
    Password = "password",
    xmpp_client:start_link(Client, Password),
    {Session, _Nick} = login_test_user(Client),
    ?debugVal(Session),
    InvalidSession = "dGVzdA==",

    Game = xmpp_client:xmpp_call(Client, ?SERVICE_BOT, ?CREATE_COMMAND_CORRECT(Session)),
    ?debugVal(Game),
    {match, [GameID]} = re:run(Game,
                                ?CREATE_RESPONSE_SUCCESS ++ ".*\"(.*)\"",
                                [{capture, all_but_first, list}]),
    ?debugVal(GameID),
    [[Client],
     {Client, 
      ?GAME_ORDER_COMMAND(Session, GameID),
       ?GAME_ORDER_RESPONSE_INVALID_DATA,
      "game order invalid data"},
     {Client, 
      ?GAME_ORDER_COMMAND_WRONG(Session, GameID),
       ?GAME_ORDER_RESPONSE_INVALID_INPUT,
       "game order invalid input"},

     {Client, 
      ?UPDATE_COMMAND_CORRECT(Session),
      ?UPD_RESPONSE_SUCCESS,
      "valid update attempt"},
     {Client, 
      ?UPDATE_COMMAND_CORRECT(InvalidSession),
      ?UPD_RESPONSE_SESSION_ERROR,
      "update attempt with invalid session"},
     {Client, 
      ?UPDATE_COMMAND_MISSING_FIELD(Session),
      ?UPD_RESPONSE_BAD_SYNTAX,
      "update command with missing fields"},
     {Client, 
      ?CREATE_COMMAND_CORRECT(Session),
      ?CREATE_RESPONSE_SUCCESS,
      "valid create command"},
     {Client, 
      ?CREATE_COMMAND_CORRECT(InvalidSession),
      ?CREATE_RESPONSE_SESSION_ERROR,
      "create attempt with invalid session"},
     {Client, 
      ?CREATE_COMMAND_MISSING_FIELDS(Session),
      ?CREATE_RESPONSE_BAD_SYNTAX,
      "create command with missing fields"},

     {Client, 
      ?RECONFIG_COMMAND(Session, GameID),
      ?RECONFIG_RESPONSE_SUCCESS,
      "successful reconfig game"},
     {Client, 
      ?RECONFIG_COMMAND(Session, "1111222"),
      ?RECONFIG_RESPONSE_INVALID_DATA,
      "invalid reconfig game data"},

     {Client, 
      ?GAME_OVERVIEW_COMMAND(Session, GameID),
      ?GAME_OVERVIEW_RESPONSE_NOT_PLAY,
      "game overview not play this game"},

     {Client, 
      ?JOIN_GAME_COMMAND(Session, GameID),
      ?JOIN_GAME_RESPONSE_SUCCESS,
      "successful join game"},
     {Client, 
      ?JOIN_GAME_COMMAND(Session, GameID),
      ?JOIN_GAME_RESPONSE_INVALID_DATA,
      "invalid join game data"},
     {Client, 
      ?GAME_OVERVIEW_COMMAND(Session, GameID),
      ?GAME_OVERVIEW_RESPONSE_SUCCESS,
      "successful game overview"},

     {Client, 
      ?GAME_ORDER_COMMAND(Session, GameID),
      ?GAME_ORDER_RESPONSE_SUCCESS,
      "game order successfully sent"},
     {Client, 
      ?SEND_OFF_GAME_MSG(Session, "not_exisiting_user", "a message"),
      ?SEND_OFF_GAME_MSG_RESPONSE_FAILED,
      "game order successfully sent"}
    ].

setup_two_user_instantiator() ->
    Password = "password",
    Client1 = user1_test_client,
    xmpp_client:start_link(Client1, Password),
    {Session1, Nick1} = login_test_user(Client1),

    Client2 = user2_test_client,
    xmpp_client:start_link(Client2, Password),
    {_Session2, Nick2} = login_test_user(Client2),

    Msg = "Hello, this is a message! :)",

    [[Client1, Client2],
     {{Client1, 
       ?SEND_OFF_GAME_MSG(Session1, Nick2, Msg),
       ?SEND_OFF_GAME_MSG_RESPONSE_SUCCESS},
      {Client2,
       ".* \(" ++ Nick1 ++ "\):\n\(.*\)\n",
       [[Nick1, Msg]]},
      "successful off game message sending"}
    ].


%%-------------------------------------------------------------------
%% @doc
%%
%%  Clean up environment after runnning tests.
%%
%%  stop exmpp client and close the session
%%-------------------------------------------------------------------
teardown([Clients|_])->
    ?debugMsg("Tearing down xmpp test."),
    % stop spewing output to tty before we kill xmpp_client
    % because it's noisy.
    error_logger:tty(false),
    lists:foreach(fun(Client) ->
                          xmpp_client:stop(Client)
                  end, Clients).


%%-------------------------------------------------------------------
%% instantiate tests
%%-------------------------------------------------------------------
instantiator([_Clients| Tests]) ->
    lists:map(fun({Client, Request, Expect, Description}) ->
                      fun() ->
                              ?debugFmt("Testing ~s.", [Description]),
                              Response = xmpp_client:xmpp_call(
                                           Client, ?SERVICE_BOT, Request),
                              ResponsePrefix = response_prefix(Expect, Response),
                              ?assertEqual(Expect, ResponsePrefix)
                      end
              end, Tests).

%%-------------------------------------------------------------------
%% instantiate two user tests
%%-------------------------------------------------------------------
two_user_instantiator([_Clients| Tests]) ->
    lists:map(fun({{Client1, Request1, Expect1},
                   {Client2, Regex, Expect2},
                   Description}) ->
                      fun() ->
                              ?debugFmt("Testing ~s.", [Description]),
                              Response1 = xmpp_client:xmpp_call(
                                           Client1, ?SERVICE_BOT, Request1),
                              ResponsePrefix1 = response_prefix(Expect1, Response1),
                              ?assertEqual(Expect1, ResponsePrefix1),

                              receive nothin -> ok after 1000 -> ok end,
                              Response2 = xmpp_client:last_received_msg(Client2),
                              {ok, Reg} = re:compile(Regex,
                                                     [dotall, {newline, anycrlf}]),
                              Result2 = re:run(Response2, Reg,
                                     [global, {capture, all_but_first, list},
                                      {newline, anycrlf}]),
                              ?assertEqual({match, Expect2}, Result2)
                      end
              end, Tests).

%%-------------------------------------------------------------------
%% @doc
%%  run echo test with ejabberd server. it send a message to a user at
%%  echo local host and get the same message
%%-------------------------------------------------------------------
echo([[Client]]) ->
    [fun() ->
             ok = xmpp_client:clear_last_received(Client),
             RMsg = xmpp_client:xmpp_call(Client, "tester@echo.localhost", ?ECHO_TEST_MSG),
             ?assertEqual(?ECHO_TEST_MSG, RMsg)
     end].


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


%%-------------------------------------------------------------------
%% @doc Creates and logges in a random user und returns the Session id.
%% @end
%%-------------------------------------------------------------------
login_test_user(Client) ->
    Nick = random_nick(),
    xmpp_client:xmpp_call(Client, ?SERVICE_BOT, ?REGISTER_COMMAND_CORRECT(Nick)),
    Response = xmpp_client:xmpp_call(Client, ?SERVICE_BOT, ?LOGIN_COMMAND_CORRECT(Nick)),
    {match, [Session]} = re:run(Response,
                                ?LOGIN_RESPONSE_SUCCESS ++ ".*\"\"(.*)\"\"",
                                [{capture, all_but_first, list}]),
    {Session, Nick}.
    

