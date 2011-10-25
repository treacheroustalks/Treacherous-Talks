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

% controller callback
-export([reply/3]).


%%-------------------------------------------------------------------
%% @doc
%% This function replies to the user depending on the result of the
%% request.
%% @end
%%
%% [@spec reply([From, To, ToHost], {Cmd, Result}, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
reply([From, To, ToHost], {register, success}, User) ->
    send_mail(To, From, ToHost,
              "Registration was successful.~n~p~n",
              [User]);
reply([From, To, ToHost], {register, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid registration data.~n~p~n",
              [Info]);

reply([From, To, ToHost], {login, success}, Session) ->
    send_mail(To, From, ToHost,
              "Login was successful. Your session is: \"~p\"~n",
              [Session]);
reply([From, To, ToHost], {login, invalid_data}, _Info) ->
    send_mail(To, From, ToHost,
              "Invalid login data.~n");

reply([From, To, ToHost], {update_user, success}, User) ->
    send_mail(To, From, ToHost,
              "User information was successfully updated.~n~p~n",
              [User]);
reply([From, To, ToHost], {update_user, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid user update information.~n~p~n",
              [Info]);

reply([From, To, ToHost], {create_game, success}, Game) ->
    send_mail(To, From, ToHost,
              "Game creation was successful.~n~p~n",
              [Game]);
reply([From, To, ToHost], {create_game, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid game creation data.~n~p~n",
              [Info]);

reply([From, To, ToHost], {Cmd, parse_error}, Error) ->
    send_mail(To, From, ToHost,
              "The command[~p] could not be interpreted correctly:~n~p~n",
              [Cmd, Error]);
reply([From, To, ToHost], unknown_command, Data) ->
    send_mail(To, From, ToHost,
              "The provided command is unknown~n~p~n",
              [Data]).


%% internal function
send_mail(From, To, ToHost, Format) ->
    io:format("[SMTP][To: ~p] ~s", [To, Format]),
    gen_smtp_client:send({From, [To], Format}, [{relay, ToHost}, {port,25}]).
send_mail(From, To, ToHost, Format, Data) ->
    send_mail(From, To, ToHost,
              io_lib:format(Format, Data)).
