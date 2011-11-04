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
%% {play_order, {OrderList, ErrorList}} |
%% {update, {ok, SessionId,[{#user.field, Value}]}} |
%% {update, Error} |
%% {create_game, {ok, Session, #game{}}} |
%% {create_game, Error} |
%% {reconfig_game, {ok, SessionId, gameid, #game{}}|
%% {reconfig_game, {error, {required_fields, RequiredFields}}}|
%% {reconfig_game,{error, {invalid_input, ErrorList}}}|
%% {game_overview, {ok, SessionId, GameId}} |
%% {game_overview, Error} |
%% {join_game, {ok, SessionId, GameId, Country} |
%% {join_game, Error} |
%% unknown_command]
%% @end
%%-------------------------------------------------------------------
% if no matched input, let it crash to detect the bug earlier
parse(BinString) when is_binary(BinString) ->
    Commands =    "("?LOGIN
                  "|"?ORDER
                  "|"?CREATE
                  "|"?RECONFIG
                  "|"?REGISTER
                  "|"?UPDATE
                  "|"?OVERVIEW
                  "|"?JOIN
                  ")(.*)END",

    {ok, MP} = re:compile(Commands, [dotall]),
    case re:run(BinString, MP, [{capture, all_but_first, binary}]) of
        {match, [Cmd, Data]} ->
            case Cmd of
                <<?LOGIN>> ->
                    {login, user_commands:parse_login(Data)};
                <<?ORDER>> ->
                    {game_move, player_orders:parse_orders(Data)};
                <<?CREATE>> ->
                    {create_game, user_commands:parse_create(Data)};
                <<?RECONFIG>> ->
                    {reconfig_game, user_commands:parse_reconfig(Data)};
                <<?REGISTER>> ->
                    {register, user_commands:parse_register(Data)};
                <<?UPDATE>> ->
                    {update_user, user_commands:parse_update(Data)};
                <<?OVERVIEW>> ->
                    {game_overview, user_commands:parse_overview(Data)};
                <<?JOIN>> ->
                    {join_game, user_commands:parse_join(Data)}
            end;
         nomatch ->
                unknown_command
    end.
