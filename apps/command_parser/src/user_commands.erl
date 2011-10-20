%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc user_command
%%%
%%% A module for recognizing user command in email body
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(user_commands).

%Export for API
-export([parse_register/1, parse_login/1]).

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include("include/records.hrl").% -record(reg_info,{})


%%------------------------------------------------------------------------------
%% @doc parse_login/1
%%
%% Parses a login string into a user record.
%% @end
%%------------------------------------------------------------------------------
parse_login(Data) ->
    Fields = ["NICKNAME", "PASSWORD"],
    Values = get_fields(Fields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, Fields}};
        false ->
            [Nick, Pw] = Values,
            {ok, #user{nick = Nick, password = Pw}}
    end.

%%------------------------------------------------------------------------------
%% @doc parse_register/1
%%
%% Parses a register string into a user record.
%% @end
%%------------------------------------------------------------------------------
parse_register(Data) ->
    Fields = ["NICKNAME", "PASSWORD", "EMAIL", "FULLNAME"],
    Values = get_fields(Fields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, Fields}};
        false ->
            [Nick, Pw, Mail, Name] = Values,
            {ok, #user{nick = Nick, password = Pw,
                       email = Mail, name = Name}}
    end.


%% Internal function
get_fields(Fields, Data) ->
    lists:map(fun(Field) ->
                      {ok, MP} = 
                          re:compile(Field ++ ":\s*(.*)\s*", [{newline, anycrlf}]),
                      case re:run(Data, MP, [{capture, all_but_first, list}]) of
                          {match, [Value]} ->
                              Value;
                          nomatch -> 
                              field_missing
                      end
              end, Fields).
