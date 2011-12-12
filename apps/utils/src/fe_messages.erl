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
%%% @author Sukumar Yethadka <sbhat7@gmail.com>
%%%
%%% @doc This modules composes replies to the user.
%%%
%%% @end
%%%
%%% @since : 9 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(fe_messages).

-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("utils/include/debug.hrl").

-export([get/2, resp/1, resp/2, resp_unhandled_error/1]).

%%-------------------------------------------------------------------
%% @doc get/2
%%
%% Composes messages for specified return values from the backend
%% @end
%% [@spec get(Result(), Value()) @end]
%%-------------------------------------------------------------------
% User registration
get({register, success}, _Val) ->
    resp("Registration was successful.~n");
get({register, invalid_data}, Error) ->
    case Error of
        nick_already_exists ->
            resp("Nick name not available.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% User login
get({login, success}, SessionId) ->
    resp("Login was successful. Your session is: \"~p\"~n", [SessionId]);
get({login, invalid_data}, Error) ->
    case Error of
        nick_not_unique ->
            resp("Duplicate nick detected!.~n");
        invalid_login_data ->
            resp("Invalid login data.~n");
        simultaneous_login ->
            resp("Simultaneous login not allowed.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Operator get DB status
get({get_db_stats, success}, _JsonList) ->
    resp("Get DB stats success.~n");
get({get_db_stats, invalid_data}, Error) ->
    case Error of
        get_stats_body_fail ->
            resp("Load Json body failed.~n");
        not_operator ->
            resp("Permission denied, not operator.~n");
        get_stats_timeout ->
            resp("Receive stats data timeout.~n");
        get_stats_tcp_error ->
            resp("TCP error.~n");
        get_stats_start_socket_fail ->
            resp("Cannot start connection.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Operator get ongoing games
get({get_games_ongoing, _}, _Data) ->
    resp("");

% Get session user
get({operator_game_overview, success}, _Val) ->
    resp("operator game overview success");
get({operator_game_overview, invalid_data}, Error) ->
    io:format(user,"#######~p~n",[Error]),
    case Error of
        {{badmatch,{error,notfound}}, _} ->
            resp("no such game in game bucket");
        _ ->
            resp("operator game overview fail")
    end;

% Get session user
get({operator_get_game_msg, success}, _Val) ->
    resp("operator get game msg success");
get({operator_get_game_msg, invalid_data}, _Val) ->
    resp("operator get game msg fail");

% Get session user
get({get_session_user, success}, Val) ->
    resp(data_format:user_to_text(Val));
get({get_session_user, invalid_data}, _Val) ->
    resp("");

% User update
get({update_user, success}, _Val) ->
    resp("User information was successfully updated.~n");
get({update_user, invalid_data}, Error) ->
    case Error of
        does_not_exist ->
            resp("Given user does not exist.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Game create
get({create_game, success}, GameId) ->
    resp("Game creation was successful. Your game ID is: \"~p\"~n", [GameId]);
get({create_game, invalid_data}, Error) ->
    case Error of
        _ ->
            resp_unhandled_error(Error)
    end;

% Get game
get({get_game, success}, _Val) ->
    resp("");
get({get_game, invalid_data}, _Val) ->
    resp("");

% Game reconfiguration
get({reconfig_game, success}, _Val) ->
    resp("Game information was successfully updated.~n");
get({reconfig_game, invalid_data}, Error) ->
    case Error of
        game_does_not_exist ->
            resp("The game you are trying to reconfigure does not exist.~n");
        game_started_already ->
            resp("Game cannot be reconfigured once its started.~n");
        not_game_creator ->
            resp("Only the creator of the game can reconfigure it.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Game join
get({join_game, success}, _Val) ->
    resp("Join game was successful.~n");
get({join_game, invalid_data}, Error) ->
    case Error of
        country_not_available ->
            resp("Selected country not available.~n");
        user_already_joined ->
            resp("You have already joined this game.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Game overview
get({game_overview, success}, _Val) ->
    resp("Game Overview~n");
get({game_overview, invalid_data}, Error) ->
    case Error of
        user_not_playing_this_game ->
            resp("Only game players can view the game overview.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Stop game
get({stop_game, success}, _Val) ->
    resp("Game was stopped successfully~n");
get({stop_game, invalid_data}, Error) ->
    resp_unhandled_error(Error);

get({get_reports, success}, _Val) ->
    resp("");
get({get_reports, invalid_data}, _Error) ->
    resp("");

get({mark_report_as_done, success}, _Val) ->
    resp("Issue marked as done~n");
get({mark_report_as_done, invalid_data}, Error) ->
    case Error of
        notfound ->
            resp("The issue could not be found.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Game order
get({game_order, success}, _Val) ->
    resp("Game order sent successfully:~n~n");
get({game_order, invalid_data}, Error) ->
    case Error of
        game_id_not_exist ->
            resp("The game you are sending orders to, does not exist.~n");
        user_not_playing_this_game ->
            resp("You cannot send orders to a game you are not playing.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Off game messaging
get({user_msg, success}, MessageId) ->
    resp("Message was sent. Message ID is: \"~p\"~n", [MessageId]);
get({user_msg, invalid_data}, Error) ->
    case Error of
        nick_not_unique ->
            resp("Error: Duplicate nick detected!.~n");
        invalid_nick ->
            resp("Error: The user does not exist.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

get({game_msg, success}, GameId) ->
    resp("Game Message was sent. Game ID is: \"~p\"~n", [GameId]);
get({game_msg, invalid_data}, Error) ->
    case Error of
        not_allowed_send_msg ->
            resp("Error:You are not allowed to send message in no press games! ~n");
        game_does_not_exist ->
            resp("Error: The game does not exist.~n");
        game_phase_not_ongoing ->
            resp("Error: Cannot send messages to a game that is not ongoing");
        _ ->
            resp_unhandled_error(Error)
    end;

get({power_msg, success}, GameId) ->
    resp("Power user game gessage was sent to game ~p.", [GameId]);
get({power_msg, invalid_data}, Error) ->
    case Error of
        game_does_not_exist ->
            resp("Error: The game does not exist.~n");
        game_phase_not_ongoing ->
            resp("Error: Cannot send messages to a game that is not ongoing");
        _ ->
            resp_unhandled_error(Error)
    end;

% Games current
get({games_current, success}, Games) ->
    resp("Found ~p games:~n~n", [length(Games)]);
get({games_current, invalid_data}, Error) ->
    resp_unhandled_error(Error);

% Games search
get({game_search, success}, Games) ->
    resp("Found ~p games~n~n", [length(Games)]);
get({game_search, invalid_data}, Error) ->
    resp_unhandled_error(Error);

%add/ remove user from moderator list
get({assign_moderator, success}, User) ->
    resp("You updated the role of ~p to ~p~n", [User#user.nick, User#user.role]);
get({assign_moderator, invalid_data}, Error) ->
    case Error of
        user_not_found ->
            resp("The nick does not exist");
        _ ->
            resp_unhandled_error(Error)
    end;


% Invalid session
get({_Cmd, invalid_session}, _Val) ->
    resp("Invalid user session. Please log in to continue.~n");

% access denied
get({Cmd, access_denied}, _Val) ->
    resp("Access denied. You are not allowed to run [~p].~n", [Cmd]);

% Command parse error
get({Cmd, parse_error}, Error) ->
    case Error of
        {error,{required_fields,FieldStrList}} ->
            resp("The command [~p] could not be interpreted correctly:~n"
                 "Required fields: ~p", [Cmd, FieldStrList]);
        {error,{invalid_input, _Val}} ->
            resp("Invalid input for the given command.~n");
        {error,{empty_query, Val}} ->
            resp("[~p] Please enter some search criteria.~n", [Val]);
        _ ->
            resp("The command [~p] could not be interpreted.~n", [Cmd])
    end;


% Push events
% Off game message
get(off_game_msg, Msg = #message{}) ->
    resp(data_format:date_to_str(Msg#message.date_created) ++ "~s:~n~s",
         [Msg#message.from_nick, Msg#message.content]);

%in game message
get(in_game_msg, GMsg =#game_message{}) ->
    Sender = GMsg#game_message.from_country,
    case is_power_user(Sender) of
        false ->
            resp(data_format:date_to_str(GMsg#game_message.date_created) ++
                 "Game:~p~nCountry:~p,~n ~s",
                 [GMsg#game_message.game_id, Sender,
                  GMsg#game_message.content]);
        _PowerUser ->
            resp(data_format:date_to_str(GMsg#game_message.date_created) ++
                 "Message from ~p~nGame: ~p~n~s",
                 [Sender, GMsg#game_message.game_id,
                  GMsg#game_message.content])
    end;

% User update
get({get_presence, success}, Val) ->
    case Val of
        user_online ->
            resp("User is ONLINE.~n");
        user_offline ->
            resp("User is OFFLINE.~n")
    end;
get({get_presence, invalid_data}, Error) ->
    case Error of
        user_not_found ->
            resp("Given user does not exist.~n");
        _ ->
            resp_unhandled_error(Error)
    end;

% Unimplemented command
get({Cmd, _Status}, _Val) ->
    resp("Messages not implemented for ~p.~n", [Cmd]);

% Unknown command
get(unknown_command, _Val) ->
    resp("The provided command is unknown.~n");

% Additional commands
get(game_overview, Val) ->
    game_overview(Val);
get(games_current, Val) ->
    get_games_in_textual_form(Val);
get(game_search, Val) ->
    get_games_in_textual_form(Val).

%%-------------------------------------------------------------------
%% @doc resp/2
%%
%% Create a formatted response with Data inserted in the Format
%% @end
%% [@spec get(Format(), Data()) @end]
%%-------------------------------------------------------------------
resp(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%%-------------------------------------------------------------------
%% @doc resp/1
%%
%% Create a formatted response
%% @end
%% [@spec get(Format(), Data()) @end]
%%-------------------------------------------------------------------
resp(Format) ->
    resp(Format, []).

%%-------------------------------------------------------------------
%% @doc resp_unhandled_error/1
%%
%% Get response for unhandled error
%% @end
%% [@spec get(Format(), Data()) @end]
%%-------------------------------------------------------------------
resp_unhandled_error(Error) ->
    resp("Unhandled error.~n~p~n", [Error]).


%% Get game overview in text format
game_overview(#game_overview{} = GOV)->
    case data_format:game_overview_to_text(GOV) of
        {_, finished, FinishedGame} ->
            finished_game_overview(FinishedGame);
        {_, _Status, GameData} ->
            normal_game_overview(GameData);
        _ ->
            lists:flatten(io_lib:format("~nCannot interpret data ~n~n~s", [GOV]))
    end.

normal_game_overview({GameInfo, Country, Game, Provinces, Units, Orders}) ->
    Msg1 = io_lib:format("You are playing as: ~s~n", [Country]),
    Msg2 = io_lib:format("Game Information: ~n~s", [GameInfo]),
    Msg3 = io_lib:format("Game configurations: ~s~n ", [Game]),
    Msg4 = io_lib:format("~nYour provinces:~n~s ~nAll units:~n~s~nYour Orders:~n~p",
                         [Provinces, Units, Orders]),
    lists:flatten(Msg1 ++ Msg2 ++ Msg3 ++ Msg4).

finished_game_overview({GameInfo, PlayerInfo, Game, FinalMap}) ->
    Msg1 = io_lib:format("Game Information: ~n~n~s", [GameInfo]),
    Msg2 = io_lib:format("Game configurations:~n~s~n ", [Game]),
    Msg3 = io_lib:format("Players in this game:~n~s~n ", [PlayerInfo]),
    Msg4 = io_lib:format("~nFinal map:~n~s", [FinalMap]),
    lists:flatten(Msg1 ++ Msg2 ++ Msg3 ++ Msg4).

%% Get games in text format
get_games_in_textual_form(Games) ->
    CurrentGames = data_format:games_list_to_text(Games),
    lists:flatten(CurrentGames).


%%-------------------------------------------------------------------
%% Internal helper functions
%%-------------------------------------------------------------------
%% ------------------------------------------------------------------
%% @doc
%% Checks if a user role is a power user (moderator or operator)
%% @spec
%% is_power_user(Role :: atom()) -> boolean()
%% @end
%% ------------------------------------------------------------------
is_power_user(Role) when Role == moderator ; Role == operator ->
    true;
is_power_user(_Role) ->
    false.
