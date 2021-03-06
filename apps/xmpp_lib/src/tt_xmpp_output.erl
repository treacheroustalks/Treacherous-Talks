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

-include("ejabberd/include/ejabberd.hrl").
-include("ejabberd/include/jlib.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("utils/include/command_parser.hrl").


% controller callback
-export([reply/3]).

%%-------------------------------------------------------------------
%% @doc
%% This function replies to the user depending on the result of the
%% request.
%% @end
%%-------------------------------------------------------------------
-spec reply([term()], Result::term(), Data::term()) -> ok.
reply([From, To], Result, Data) ->
    send_chat(From, To, get_reply(Result, Data)).

%% Compose replies based on data returned from backend
%% Only Results that need additional information are matched
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

% Unknown commands
get_reply(unknown_command, Data) ->
    Msg = fe_messages:get(unknown_command, Data),
    fe_messages:resp(Msg ++ "Supported commands are:~n" ++ ?SUPPORTED_COMMANDS);
% Normal reply
get_reply(Result, Data) ->
    fe_messages:get(Result, Data).


%%-------------------------------------------------------------------
%% @doc
%%  send the message to a user as a chat message
%%-------------------------------------------------------------------
send_chat(From, To, Body) ->
    send_message(From, To, "chat", Body).


%%-------------------------------------------------------------------
%% @ doc
%% send a prepared xml chat message and route it to the user
%%-------------------------------------------------------------------
send_xml_message(From, To, XmlBody) ->
    ejabberd_router:route(From, To, XmlBody).

%%-------------------------------------------------------------------
%% @ doc
%% prepare a xml chat message and route it to the user
%%-------------------------------------------------------------------
send_message(From, To, TypeStr, BodyStr) ->
    ?INFO_MSG("Sending message type [~s]~n"
              "From ~p~n"
              "To ~p~n"
              "Body \"~s\".~n",
              [TypeStr, From#jid.user, To#jid.user, BodyStr]),
    send_xml_message(From, To,
                     xml_message:prepare(From, To, TypeStr, BodyStr)).

