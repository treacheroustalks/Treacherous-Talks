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
%% [@spec reply([From, To, ToHost], {Cmd, Result}, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
%% user registration
reply([From, To, ToHost], {register, success}, User) ->
    send_mail(To, From, ToHost,
              "Registration was successful.~n~p~n",
              [User]);
reply([From, To, ToHost], {register, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid registration data.~n~p~n",
              [Info]);
%% user login
reply([From, To, ToHost], {login, success}, Session) ->
    send_mail(To, From, ToHost,
              "Login was successful. Your session is: \"~p\"~n",
              [Session]);
reply([From, To, ToHost], {login, invalid_data}, _Info) ->
    send_mail(To, From, ToHost,
              "Invalid login data.~n");
%% user update
reply([From, To, ToHost], {update_user, success}, User) ->
    send_mail(To, From, ToHost,
              "User information was successfully updated.~n~p~n",
              [User]);
reply([From, To, ToHost], {update_user, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid user update information.~n~p~n",
              [Info]);
%% game create
reply([From, To, ToHost], {create_game, success}, Game) ->
    send_mail(To, From, ToHost,
              "Game creation was successful. Your game ID is: \"~p\"~n",
              [Game]);
reply([From, To, ToHost], {create_game, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid game creation data.~n~p~n",
              [Info]);
%% game reconfiguration
reply([From, To, ToHost], {reconfig_game, success}, Game) ->
    send_mail(To, From, ToHost,
              "Game information was successfully updated.~n~p~n",
              [Game]);
reply([From, To, ToHost], {reconfig_game, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid reconfig game information.~n~p~n",
              [Info]);
%% game join
reply([From, To, ToHost], {join_game, success}, Info) ->
    send_mail(To, From, ToHost,
              "Join game was successful.~n~p~n",
              [Info]);
reply([From, To, ToHost], {join_game, invalid_data}, Error) ->
    send_mail(To, From, ToHost,
              "Invalid join game data.~n~p~n",
              [Error]);
%% game overview
reply([From, To, ToHost], {game_overview, success}, {ok, GOV}) ->
    Info = game_overview(GOV),
    send_mail(To, From, ToHost,
              "\nGame Overview:\n" ++ Info);
reply([From, To, ToHost], {game_overview, invalid_data},
                          user_not_play_this_game) ->
    send_mail(To, From, ToHost,
              "You do not play this game");
%% error
reply([From, To, ToHost], {Cmd, invalid_session}, Info) ->
    send_mail(To, From, ToHost,
              "[~p]Invalid user session.~n~p~n",
              [Cmd, Info]);
reply([From, To, ToHost], {Cmd, parse_error}, Error) ->
    send_mail(To, From, ToHost,
              "The command[~p] could not be interpreted correctly:~n~p~n",
              [Cmd, Error]);
reply([From, To, ToHost], unknown_command, _Data) ->
    send_mail(To, From, ToHost,
              "The provided command is unknown.~n"
              "Supported commands are:~n"
              ?SUPPORTED_COMMANDS).


%% internal function
send_mail(From, To, ToHost, Format) ->
    io:format("[SMTP][To: ~p] ~s", [To, Format]),
    gen_smtp_client:send({From, [To], Format}, [{relay, ToHost}, {port,25}]).
send_mail(From, To, ToHost, Format, Data) ->
    send_mail(From, To, ToHost,
              io_lib:format(Format, Data)).


%%-------------------------------------------------------------------
%% @doc
%% provide game infomation for user which extract from
%%  game record, country and current map of the game
%%  input is the game_overview record and it returns information in string
%% @end
%%-------------------------------------------------------------------
game_overview(#game_overview{} = GOV)->
    {Country, Game, Provinces, Units} = data_format:game_overview_to_text(GOV),
    Msg1 = io_lib:format("~nYou are playing as ~s ~nGame Information:~n",
                         [Country]),
    Msg2 = io_lib:format("in game_overview after msg: ~s~n ", [Game]),
    Msg3 = io_lib:format("~nYour provinces:~n~s ~nAll units:~n~s",
                          [Provinces, Units]),
    lists:flatten(Msg1 ++ Msg2 ++ Msg3).