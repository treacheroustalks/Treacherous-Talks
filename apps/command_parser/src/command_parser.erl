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
%% unknown_command
%% @end]
%%-------------------------------------------------------------------
parse(BinString) when is_binary(BinString) ->
    case get_type(BinString) of
        {register, Data} ->
            {register, user_commands:parse_register(Data)};
        {login, Data} ->
            {login, user_commands:parse_login(Data)};
        unknown_command ->
            unknown_command
    end.


%% Internal function
get_type(BinString) ->
    Commands = "(REGISTER|LOGIN)",
    TypeReg = Commands ++ "(.*)END",
    {ok, MP} = re:compile(TypeReg, [dotall]),
    case re:run(BinString, MP, [{capture, all_but_first, binary}]) of
        {match, [Cmd, Input]} ->
            case Cmd of
                <<"REGISTER">> ->
                    {register, Input};
                <<"LOGIN">> ->
                    {login, Input}
            end;
         nomatch ->
                unknown_command
    end.
