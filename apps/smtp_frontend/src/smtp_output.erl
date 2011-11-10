%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
-export([reply/3]).

-define(SUPPORTED_COMMANDS,
        "REGISTER, LOGIN, UPDATE, CREATE, OVERVIEW").

%%-------------------------------------------------------------------
%% @doc
%% This function replies to the user depending on the result of the
%% request.
%% @end
%%
%% [@spec reply([From, To, ToHost], {Cmd, Status}=Result, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
reply([From, To, ToHost], Result, Data) ->
    % Note: From and To are interchanged when sending the mail
    send_mail(To, From, ToHost, get_reply(Result, Data)).

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
%% Compose replies based on data returned from backend
%% Only Results that need additional information are matched
get_reply({game_overview, success}, Data) ->
    Msg = fe_messages:get({game_overview, success}, Data),
    GameOverview = fe_messages:get(game_overview, Data),
    fe_messages:resp(Msg ++ GameOverview);
get_reply(unknown_command, Data) ->
    Msg = fe_messages:get(unknown_command, Data),
    fe_messages:resp(Msg ++ "Supported commands are:~n" ++ ?SUPPORTED_COMMANDS);
get_reply(Result, Data) ->
    fe_messages:get(Result, Data).

%% Send mail to specified user
send_mail(From, To, ToHost, Reply) ->
    io:format("[SMTP][To: ~p] ~s", [To, Reply]),
    gen_smtp_client:send({From, [To], Reply}, [{relay, ToHost}, {port,25}]).