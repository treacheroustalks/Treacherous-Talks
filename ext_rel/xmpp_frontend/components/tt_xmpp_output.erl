%%%===================================================================
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Jan Daniel Bothma <jbothma@gmail.com>
%%%
%%% @doc Callback module for controller_app to pass responses to
%%% requests via the xmpp_frontend.
%%%
%%% @see controller_app
%%%
%%% @since : 26 Oct 2011 by Bermuda Triangle
%%%==================================================================
-module(tt_xmpp_output).

-include("ejabberd.hrl").
-include("jlib.hrl").
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
%% [@spec reply([From, To], {Cmd, Result}, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
reply([From, To], {register, success}, User) ->
    send_chat(From, To,
              "Registration was successful.~n~p~n",
              [User]);
reply([From, To], {register, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid registration data.~n~p~n",
              [Info]);

reply([From, To], {login, success}, Session) ->
    send_chat(From, To,
              "Login was successful. Your session is: \"~p\"~n",
              [Session]);
reply([From, To], {login, invalid_data}, _Info) ->
    send_chat(From, To,
              "Invalid login data.~n");

reply([From, To], {update_user, success}, User) ->
    send_chat(From, To,
              "User information was successfully updated.~n~p~n",
              [User]);
reply([From, To], {update_user, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid user update information.~n~p~n",
              [Info]);

reply([From, To], {create_game, success}, GameID) ->
    send_chat(From, To,
              "Game creation was successful. Your game ID is: \"~p\"~n",
              [GameID]);
reply([From, To], {create_game, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid game creation data.~n~p~n",
              [Info]);

reply([From, To], {reconfig_game, success}, Game) ->
    send_chat(From, To,
              "Game information was successfully updated.~n~p~n",
              [Game]);
reply([From, To], {reconfig_game, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid reconfig game information.~n~p~n",
              [Info]);

reply([From, To], {join_game, success}, Info) ->
    send_chat(From, To,
              "Join game was successful.~n~p~n", [Info]);
reply([From, To], {join_game, invalid_data}, Error) ->
    send_chat(From, To,
              "Invalid join game data.~n~p~n", [Error]);

reply([From, To], {game_overview, success}, GOV) ->
    Info = game_overview(GOV),
    send_message(From, To,
                 "chat", "\nGame Overview:\n" ++ Info);
reply([From, To], {game_overview, invalid_data},
      user_not_playing_this_game) ->
    send_chat(From, To,
              "You are not playing this game");

reply([From, To], {Cmd, invalid_session}, Info) ->
    send_chat(From, To,
              "[~p]Invalid user session.~n~p~n",
              [Cmd, Info]);

reply([From, To], {Cmd, parse_error}, Error) ->
    send_chat(From, To,
              "The command [~p] could not be interpreted correctly:~n~s~n",
              [Cmd, parse_error_msg(Error)]);

reply([From, To], unknown_command, _Data) ->
    send_chat(From, To,
              "The provided command is unknown.~n"
              "Supported commands are:~n"
              ?SUPPORTED_COMMANDS).


%%===================================================================
%% Internal Functions
%%===================================================================
parse_error_msg({error,{required_fields,FieldStrList}}) ->
    io_lib:format("Required fields: ~p", [FieldStrList]);
parse_error_msg({error,{invalid_input,FieldStrList}}) ->
    io_lib:format("Invalid input to the following: ~p", [FieldStrList]).


%%-------------------------------------------------------------------
%% @ doc
%%  send the message to a user as a chat message
%%-------------------------------------------------------------------
send_chat(From, To, Format) ->
    send_chat(From, To, Format, []).

send_chat(From, To, Format, Data) ->
    Body = io_lib:format(Format, Data),
    send_message(From, To, "chat", Body).


%%-------------------------------------------------------------------
%% @ doc
%%  prepare the xml chat mesage and route it to the user
%%-------------------------------------------------------------------
send_message(From, To, TypeStr, BodyStr) ->
    ?INFO_MSG("Sending message type [~s]~n"
              "From ~p~n"
              "To ~p~n"
              "Body \"~s\".~n",
              [TypeStr, From#jid.user, To#jid.user, BodyStr]),
    XmlBody = {xmlelement, "message",
               [{"type", TypeStr},
                {"from", jlib:jid_to_string(From)},
                {"to", jlib:jid_to_string(To)}],
               [{xmlelement, "body", [],
                 [{xmlcdata, strip_bom(BodyStr)}]}]},
    ejabberd_router:route(From, To, XmlBody).


%%-------------------------------------------------------------------
%% @ doc
%%  strip the BOM or Byte Order Mark from the beginning of the body
%%-------------------------------------------------------------------
strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

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
