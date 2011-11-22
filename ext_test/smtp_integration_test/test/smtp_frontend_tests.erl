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
-define(OFF_GAME_MSG_MSG(Session, ToNick),
        "MESSAGE\n"
        "SESSION: " ++ Session ++ "\n"
        "TO: " ++ ToNick ++ "\n"
        "CONTENT: hello world\n"
        "END").

%%------------------------------------------
%% Responses
%%------------------------------------------

-define(LOGIN_RESPONSE_SUCCESS, "Login was successful. Your session is:").
-define(CREATE_RESPONSE_SUCCESS, "Game creation was successful. Your game ID is:").

%%------------------------------------------
%% Tests
%%------------------------------------------

% Two accounts using the same email address but different nicks.
% Both register, login, then one sends a message to the other.
% We check that the message content is found in the mailbox after sending.
online_msg_recv_test() ->
    Nick1 = random_nick(),
    Nick2 = random_nick(),

    register(Nick1),
    register(Nick2),

    Session1 = login(Nick1),
    Session2 = login(Nick2),

    imap_client:empty_mailbox(?MAIL_USERNAME),
    % from nick 1 to nick 2
    SendMessageMsg = ?OFF_GAME_MSG_MSG(Session1, Nick2),
    smtp_test_utils:send_mail(SendMessageMsg),

    % Message 1 and 2 in the inbox should be confirmation of sending
    % and the actual message. The order is not guaranteed.
    {_From, _To, _Subject, Message2} =
        imap_client:read_nth(?MAIL_USERNAME, 2),
    {_From, _To, _Subject, Message1} =
        imap_client:read_nth(?MAIL_USERNAME, 1),
    ?debugFmt("~nMessage 1 = ~p~n"
              "Message 2 = ~p~n",
              [Message1, Message2]),
    Idx1 = string:str(Message1, "hello world"),
    Idx2 = string:str(Message2, "hello world"),
    Found = 0 /= (Idx1 bor Idx2),
    ?assert(Found).



big_test() ->
    Nick = random_nick(),

    % register
    %----------
    register(Nick),

    % login using nick
    %-----------------
    Session = login(Nick),

    % create game using session
    %---------------------------
    imap_client:empty_mailbox(?MAIL_USERNAME),
    CreateGameMsg = ?CREATE_GAME_MSG(Session),
    smtp_test_utils:send_mail(CreateGameMsg),
    {_From, _To, _Subject, CreateGameReply} =
        imap_client:read_nth(?MAIL_USERNAME, 1),
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
        imap_client:read_nth(?MAIL_USERNAME, 1),
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

register(Nick) ->
    imap_client:empty_mailbox(?MAIL_USERNAME),
    smtp_test_utils:send_mail(?REGISTER_MSG(Nick)),
    {_From, _To, _Subject, RegReply} = imap_client:read_nth(?MAIL_USERNAME, 1),
    ?assertEqual("Registration was successful.", RegReply).

login(Nick) ->
    imap_client:empty_mailbox(?MAIL_USERNAME),
    LoginMsg = ?LOGIN_MSG(Nick),
    smtp_test_utils:send_mail(LoginMsg),
    {_From, _To, _Subject, LoginReply} = imap_client:read_nth(?MAIL_USERNAME, 1),
    {match, [Session]} = re:run(LoginReply,
                                ?LOGIN_RESPONSE_SUCCESS ++ ".*\"\"(.*)\"\"",
                                [{capture, all_but_first, list}]),
    ?assertEqual(?LOGIN_RESPONSE_SUCCESS, string:substr(LoginReply,1,38)),
    Session.
