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

%% Exports for eunit
-export([get_type/1]).


%%-------------------------------------------------------------------
%% @doc
%% Gets a binary string and parses it into a command and the
%% correspondig value.
%% @end
%%
%% [@spec parse(BinString:binary()) ->
%% {register, {ok, #user{}}} |
%% {register, Error} |
%% {login, {ok, #user{}}} |
%% {login, Error} |
%% {update, {ok, #user{}}} |
%% {update, Error} |
%% {create, {ok, #user{}}} |
%% {create, Error} |
%% unknown_command
%% @end]
%%-------------------------------------------------------------------
parse(BinString) when is_binary(BinString) ->
    case get_type(BinString) of
        {login, Data} ->
            {login, user_commands:parse_login(Data)};
        {create_game, Data} ->
            {create_game, user_commands:parse_create(Data)};
        {register, Data} ->
            {register, user_commands:parse_register(Data)};
        {update_user, Data} ->
            {update_user, user_commands:parse_update(Data)};
        unknown_command ->
            unknown_command
    end.


%% Internal function
get_type(BinString) ->
    Commands = "(LOGIN|CREATE|REGISTER|UPDATE)",
    TypeReg = Commands ++ "(.*)END",
    {ok, MP} = re:compile(TypeReg, [dotall]),
    case re:run(BinString, MP, [{capture, all_but_first, binary}]) of
        {match, [Cmd, Input]} ->
            case Cmd of
                <<"LOGIN">> ->
                    {login, Input};
                <<"CREATE">> ->
                    {create_game, Input};
                <<"REGISTER">> ->
                    {register, Input};
                <<"UPDATE">> ->
                    {update_user, Input}
            end;
         nomatch ->
                unknown_command
    end.
