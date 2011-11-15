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
%%%------------------------------------------------------------------
-module(smtp_frontend_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAIL_USERNAME, smtp_test_utils:from_address()).

%%------------------------------------------
%% Requests
%%------------------------------------------

-define(REGISTER_MSG(Nick),
        "REGISTER\n"
        "NICKNAME: " ++ Nick ++ "\n"
        "FULLNAME: testname\n"
        "EMAIL:    " ++ ?MAIL_USERNAME ++ "\n"
        "PASSWORD: testpass\n"
        "END").
-define(LOGIN_MSG(Nick),
        "LOGIN\n"
        "NICKNAME: " ++ Nick ++ "\n"
        "PASSWORD: testpass\n"
        "END").
-define(CREATE_GAME_MSG(Session),
        "CREATE\n"
        "SESSION: " ++ Session ++ "\n"
        "GAMENAME: mygame55\n"
        "PRESSTYPE: none\n"
        "ORDERCIRCLE: 1D\n"
        "RETREATCIRCLE: 1D\n"
        "GAINLOSTCIRCLE: 1D\n"
        "WAITTIME: 1D\n"
        "END").
-define(JOIN_GAME_MSG(Session, GameID),
        "JOIN\n"
        "SESSION: " ++ Session ++ "\n"
        "GAMEID: " ++ GameID ++ "\n"
        "COUNTRY: italy\n"
        "END").

%%------------------------------------------
%% Responses
%%------------------------------------------

-define(LOGIN_RESPONSE_SUCCESS, "Login was successful. Your session is:").
-define(CREATE_RESPONSE_SUCCESS, "Game creation was successful. Your game ID is:").

%%------------------------------------------
%% Tests
%%------------------------------------------

big_test() ->
    Nick = random_nick(),

    % register
    %----------
    imap_client:empty_mailbox(?MAIL_USERNAME),
    RegMsg = ?REGISTER_MSG(Nick),
    smtp_test_utils:send_mail(RegMsg),
    {From, To, Subject, RegReply} = imap_client:read_first(?MAIL_USERNAME),
    ?debugVal({From, To, Subject, RegReply}),
    ?assertEqual("Registration was successful.", RegReply),

    % login using nick
    %-----------------
    imap_client:empty_mailbox(?MAIL_USERNAME),
    LoginMsg = ?LOGIN_MSG(Nick),
    smtp_test_utils:send_mail(LoginMsg),
    {_From, _To, _Subject, LoginReply} = imap_client:read_first(?MAIL_USERNAME),
    {match, [Session]} = re:run(LoginReply,
                                ?LOGIN_RESPONSE_SUCCESS ++ ".*\"\"(.*)\"\"",
                                [{capture, all_but_first, list}]),
    %?debugVal(Session),
    ?assertEqual(?LOGIN_RESPONSE_SUCCESS, string:substr(LoginReply,1,38)),

    % create game using session
    %---------------------------
    imap_client:empty_mailbox(?MAIL_USERNAME),
    CreateGameMsg = ?CREATE_GAME_MSG(Session),
    smtp_test_utils:send_mail(CreateGameMsg),
    {_From, _To, _Subject, CreateGameReply} =
        imap_client:read_first(?MAIL_USERNAME),
    {match, [GameID]} = re:run(CreateGameReply,
                                ?CREATE_RESPONSE_SUCCESS ++ ".*\"(.*)\"",
                                [{capture, all_but_first, list}]),
    %?debugVal(GameID),
    ?assertEqual(?CREATE_RESPONSE_SUCCESS, string:substr(CreateGameReply,1,46)),

    % join using game id and session
    %-------------------------------
    imap_client:empty_mailbox(?MAIL_USERNAME),
    JoinGameMsg = ?JOIN_GAME_MSG(Session, GameID),
    smtp_test_utils:send_mail(JoinGameMsg),
    {_From, _To, _Subject, JoinGameReply} =
        imap_client:read_first(?MAIL_USERNAME),
    ?assertEqual("Join game was successful.", JoinGameReply),

    %shut up now that the test is over.
    error_logger:tty(false).


%%------------------------------------------
%% Internal functions
%%------------------------------------------

random_nick() ->
    "testNick" ++ integer_to_list(random_id()).

random_id() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]).
