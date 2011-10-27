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

% controller callback
-export([reply/3]).

-define(SUPPORTED_COMMANDS,
        "REGISTER, LOGIN, UPDATE, CREATE").


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

reply([From, To], {create_game, success}, Game) ->
    send_chat(From, To,
              "Game creation was successful.~n~p~n",
              [Game]);
reply([From, To], {create_game, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid game creation data.~n~p~n",
              [Info]);

reply([From, To], {reconfig_game, success}, Game) ->
    send_chat(To, From,
              "Game information was successfully updated.~n~p~n",
              [Game]);
reply([From, To], {reconfig_game, invalid_data}, Info) ->
    send_chat(To, From,
              "Invalid game update information.~n~p~n",
              [Info]);



reply([From, To], {Cmd, parse_error}, Error) ->
    send_chat(From, To,
              "The command [~p] could not be interpreted correctly:~n~s~n",
              [Cmd, parse_error_msg(Error)]);

reply([From, To], unknown_command, Data) ->
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
