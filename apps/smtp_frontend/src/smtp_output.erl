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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc This modules sends replies to the user.
%%%
%%% @end
%%%
%%% @since : 25 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(smtp_output).

-include_lib("datatypes/include/game.hrl").
-include_lib("utils/include/command_parser.hrl").

% controller callback
-export([reply/3, send_mail/3]).

-include_lib("utils/include/debug.hrl").

%%-------------------------------------------------------------------
%% @doc
%% This function replies to the user depending on the result of the
%% request.
%% @end
%%-------------------------------------------------------------------
-spec reply([string()], Result::term(), Data::term()) -> ok.
reply([From, To], Result, Data) ->
    % Note: From and To are interchanged when sending the mail
    send_mail(To, From, get_reply(Result, Data)).


%% Send mail to specified user
-spec send_mail(string(), string(), string()) -> ok.
send_mail(From, To, Body) ->
    Mail = lists:flatten(io_lib:format("From: ~s\r\n"
                                       "To: ~s\r\n"
                                       "Subject: Treacherous Talks\r\n\r\n"
                                       "~s",
                                       [From, To, Body])),
    ?DEBUG("[SMTP][From: ~p][To: ~p]~n~s", [From, To, Mail]),
    RelayHost = case application:get_env(relay_host) of
        {ok, Host} ->
            Host;
        undefined ->
            "mail.pcs"  %slightly better form of hardcoding - can be set in app config.
    end,
    _SendResult = gen_smtp_client:send({From, [To], Mail},
                                      [{relay, RelayHost},
                                       {port,25},
                                       {tls, never}]),
    ?DEBUG("Send result was ~p~n", [_SendResult]).


%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
%% Compose replies based on data returned from backend
%% Only Results that need additional information are matched
%%-------------------------------------------------------------------
% Game overview
get_reply({game_overview, success}, Data) ->
    Msg = fe_messages:get({game_overview, success}, Data),
    GameOverview = fe_messages:get(game_overview, Data),
    fe_messages:resp(Msg ++ GameOverview);
% View current game(s)
get_reply({games_current, success}, Data) ->
    Msg = fe_messages:get({games_current, success}, Data),
    GamesCurrent = fe_messages:get(games_current, Data),
    fe_messages:resp(Msg ++ GamesCurrent);
% Search game(s)
get_reply({game_search, success}, Data) ->
    Msg = fe_messages:get({game_search, success}, Data),
    GameSearch = fe_messages:get(game_search, Data),
    fe_messages:resp(Msg ++ GameSearch);

% Unknown command
get_reply(unknown_command, Data) ->
    Msg = fe_messages:get(unknown_command, Data),
    fe_messages:resp(Msg ++ "Supported commands are:~n" ++ ?SUPPORTED_COMMANDS);
% Normal reply
get_reply(Result, Data) ->
    fe_messages:get(Result, Data).
