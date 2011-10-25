%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
-export([parse/1]).

-include("command_parser.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Gets a binary string and parses it into a command and the
%% correspondig value.
%%
%%
%% [@spec parse(BinString:binary()) ->
%% {register, {ok, #user{}}} |
%% {register, Error} |
%% {login, {ok, #user{}}} |
%% {login, Error} |
%% {update, {ok, UserNick,[{#user.field, Value}]}} |
%% {update, Error} |
%% {create_game, {ok, #user{}}} |
%% {create_game, Error} |
%% {reconfig_game, {ok, gameid, #game{}}|
%% {reconfig_game, {error, {required_fields, RequiredFields}}}|
%% {reconfig_game,{error, {invalid_input, ErrorList}}}|
%% unknown_command]
%% @end
%%-------------------------------------------------------------------
% if no matched input, let it crash to detect the bug earlier
parse(BinString) when is_binary(BinString) ->
    Commands =   "(" ++ ?LOGIN
               ++"|" ++ ?CREATE
               ++"|" ++ ?RECONFIG
               ++ "|" ++ ?REGISTER
               ++ "|" ++ ?UPDATE
               ++ ")(.*)END",

    {ok, MP} = re:compile(Commands, [dotall]),
    case re:run(BinString, MP, [{capture, all_but_first, binary}]) of
        {match, [Cmd, Data]} ->
            case Cmd of
                <<?LOGIN>> ->
                    {login, user_commands:parse_login(Data)};
                <<?CREATE>> ->
                    {create_game, user_commands:parse_create(Data)};
                <<?RECONFIG>> ->
                    {reconfig_game, user_commands:parse_reconfig(Data)};
                <<?REGISTER>> ->
                    {register, user_commands:parse_register(Data)};
                <<?UPDATE>> ->
                    {update_user, user_commands:parse_update(Data)}
            end;
         nomatch ->
                unknown_command
    end.