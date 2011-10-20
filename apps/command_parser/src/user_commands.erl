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
-export([parse_create/1, parse_update/1, parse_register/1, parse_login/1]).

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include_lib("datatypes/include/game.hrl").% -record(user,{})
-include("include/records.hrl").% -record(reg_info,{})


%%------------------------------------------------------------------------------
%% @doc parse_login/1
%%
%% Parses a login string into a user record.
%% @end
%%------------------------------------------------------------------------------
parse_login(Data) ->
    RequiredFields = ["NICKNAME", "PASSWORD"],
    Values = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Nick, Pw] = Values,
            {ok, #user{nick = Nick, password = Pw}}
    end.


%%------------------------------------------------------------------------------
%% @doc parse_create/1
%%
%% Parses a create string into a game record.
%%
%% @end
%%------------------------------------------------------------------------------
parse_create(Data) ->
    RequiredFields = ["GAMENAME", "PRESSTYPE", "ORDERCIRCLE", "RETREATCIRCLE",
                      "GAINLOSTCIRCLE", "WAITTIME"],
    Values = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Name, Press, OrdPhase, RetPhase, BldPhase, WaitTime] = Values,
            {ok, #game{name = Name, press = Press, order_phase = OrdPhase,
                       retreat_phase = RetPhase, build_phase = BldPhase,
                       waiting_time = WaitTime, creator_id = undefined}}
    end.


%%------------------------------------------------------------------------------
%% @doc parse_register/1
%%
%% Parses a register string into a user record.
%%
%% @TODO forbid invalid symbols in registration
%% @end
%%------------------------------------------------------------------------------
parse_register(Data) ->
    RequiredFields = ["NICKNAME", "PASSWORD", "EMAIL", "FULLNAME"],
    Values = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Nick, Pw, Mail, Name] = Values,
            {ok, #user{nick = Nick, password = Pw,
                       email = Mail, name = Name}}
    end.


%%------------------------------------------------------------------------------
%% @doc parse_update/1
%%
%% Parses a update string into a user record.
%%
%% @TODO allow user to update particular fields rather than entire
%% @end
%%------------------------------------------------------------------------------
parse_update(Data) ->
    RequiredFields = ["NICKNAME", "PASSWORD", "FULLNAME"],
    Values = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Nick, Pw, Name] = Values,
            {ok, #user{nick = Nick, password = Pw,
                      name = Name}}
    end.


%% Internal function
%%------------------------------------------------------------------------------
%%
%% Get required field value from field name
%%
%% @end
%%------------------------------------------------------------------------------
get_required_fields(Fields, Data) ->
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
