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
%%% @doc Library to parse user commands..
%%%
%%% @end
%%%
%%% @since : 20 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(command_parser).


%% Public interface
-export([parse/2]).

-include("command_parser.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/message.hrl").

%%-------------------------------------------------------------------
%% @TODO at least the spec function arity is wrong
%% @TODO the `{game_order,...}' return case is missing in the specs. what else?
%% @doc
%% Gets a binary string and parses it into a command and the
%% correspondig value.
%% @end
%%-------------------------------------------------------------------
% if no matched input, let it crash to detect the bug earlier
-spec parse(BinString::binary(), Client::atom()) ->
          {register, {ok, #user{}}} |
          {register, Error::term()} |
          {login, {ok, #user{}}} |
          {login, Error::term()} |
          {logout, {ok, string(), atom()}} |
          {logout, Error::term()} |
          {play_order, {[term()], [term()]}} |
          {update, {ok, SessionId::string(),[{UserRecFieldPos::integer(), Value::term()}]}} |
          {update, Error::term()} |
          {create_game, {ok, Session::string(), #game{}}} |
          {create_game, Error::term()} |
          {reconfig_game, {ok, SessionId::string(), {gameid, #game{}}}} |
          {reconfig_game, {error, {required_fields, []}}}|
          {reconfig_game,{error, {invalid_input, [term()]}}}|
          {game_overview, {ok, SessionId::string(), GameId::integer()}} |
          {game_overview, Error::term()} |
          {games_current, {ok, SessionId::string(), atom()}} |
          {games_current, Error::term()} |
          {join_game, {ok, SessionId::string(), {GameId::integer(), Country::atom()}}} |
          {join_game, Error::term()} |
          {game_search, {ok, SessionId::string(), Query::string()}} |
          {game_search, Error::term()} |
          {user_msg, {ok, SessionId::string(), #frontend_msg{}}} |
          {user_msg, {error, {required_fields, list()}}} |
          {user_msg, {error, {invalid_input, list()}}} |
          {game_msg, {ok, SessionId::string(), #frontend_msg{}}} |
          {game_msg, {error, {required_fields, list()}}} |
          {game_msg, {error, {invalid_input, list()}}} |
          {get_session_user, {ok, SessionId::string(), no_arg}} |
          {get_session_user, {error, {required_fields, list()}}} |
          {get_session_user, {error, {invalid_input, list()}}} |
          {power_msg, {ok, SessionId::string(), #frontend_msg{}}} |
          {power_msg, {error, {required_fields, list()}}} |
          {power_msg, {error, {invalid_input, list()}}} |
          {send_report, {ok, SessionId::string(), #report_message{}}} |
          {send_report, {error, {required_fields, list()}}} |
          {send_report, {error, {invalid_input, list()}}} |
          atom().
parse(BinString, Client) when is_binary(BinString) ->
    Commands =    "("?LOGIN
                  "|"?LOGOUT
                  "|"?ORDER
                  "|"?CREATE
                  "|"?RECONFIG
                  "|"?REGISTER
                  "|"?UPDATE
                  "|"?OVERVIEW
                  "|"?VIEWCURRENTGAMES
                  "|"?JOIN
                  "|"?SEARCH
                  "|"?MESSAGE
                  "|"?GETPROFILE
                  "|"?POWERMESSAGE
                  "|"?GETPRESENCE
                  "|"?REPORTPLAYER
                  "|"?REPORTISSUE
                  ")(.*)END",

    {ok, MP} = re:compile(Commands, [dotall]),
    case re:run(BinString, MP, [{capture, all_but_first, binary}]) of
        {match, [Cmd, Data]} ->
            case Cmd of
                <<?LOGIN>> ->
                    {login, user_commands:parse_login(Data)};
                <<?LOGOUT>> ->
                    {logout, user_commands:parse_logout(Data)};
                <<?ORDER>> ->
                    {game_order, player_orders:parse_orders(Data)};
                <<?CREATE>> ->
                    {create_game, user_commands:parse_create(Data)};
                <<?RECONFIG>> ->
                    {reconfig_game, user_commands:parse_reconfig(Data)};
                <<?REGISTER>> ->
                    ParsedData = case user_commands:parse_register(Data) of
                                     {ok, User= #user{}} ->
                                         case User#user.channel of
                                             undefined ->
                                                 {ok,
                                                  User#user{channel=Client}};
                                             _ ->
                                                 {ok, User}
                                         end;
                                     Other ->
                                         Other
                                 end,
                    {register, ParsedData};
                <<?UPDATE>> ->
                    {update_user, user_commands:parse_update(Data)};
                <<?OVERVIEW>> ->
                    {game_overview, user_commands:parse_overview(Data)};
                <<?VIEWCURRENTGAMES>> ->
                    {games_current, user_commands:parse_games_current(Data)};
                <<?JOIN>> ->
                    {join_game, user_commands:parse_join(Data)};
                <<?SEARCH>> ->
                    {game_search, user_commands:parse_game_search(Data)};
                <<?GETPROFILE>> ->
                    {get_session_user, user_commands:parse_get_session_user(Data)};
                <<?MESSAGE>> ->
                    Pattern =  ?GAMEID,
                    {ok, MP1} = re:compile(Pattern, [dotall]),
                    case re:run(Data, MP1, [{capture, all_but_first, list}]) of
                        {match, _} ->
                            {game_msg, user_commands:parse_game_msg(Data)};
                        nomatch ->
                            {user_msg, user_commands:parse_user_msg(Data)}
                    end;
                <<?POWERMESSAGE>> ->
                    {power_msg, user_commands:parse_game_msg(Data)};
                <<?GETPRESENCE>> ->
                    {get_presence, user_commands:parse_get_presence(Data)};
                <<?REPORTPLAYER>> ->
                    {send_report, user_commands:parse_send_report(Data, report_player)};
                <<?REPORTISSUE>> ->
                    {send_report, user_commands:parse_send_report(Data, report_issue)}
            end;
         nomatch ->
                unknown_command
    end.
