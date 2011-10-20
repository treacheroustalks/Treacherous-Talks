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
%%-------------------------------------------------------------------
parse(String) when is_binary(String) ->
    case get_type(String) of
        register ->
            {register, user_commands:parse_register(String)};
        login ->
            {login, user_commands:parse_login(String)};
        unknown_command ->
            unknown_command
    end.


get_type(String) ->
    TypeReg = "(REGISTER|LOGIN)",
    case re:run(String, TypeReg, [{capture, first, binary}]) of
        {match, [Match]} ->
            case Match of
                <<"REGISTER">> ->
                    register
            end;
         nomatch ->
                unknown_command
    end.
