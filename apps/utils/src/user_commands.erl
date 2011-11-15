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
%%% @doc user_command
%%%
%%% A module for recognizing user command in email body
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(user_commands).

% Export for API
-export([parse_create/1,
         parse_update/1,
         parse_register/1,
         parse_login/1,
         parse_reconfig/1,
         parse_overview/1,
         parse_join/1]).

% Export for eunit
-export([parse_time_format/1, is_valid_value/2, get_error_list/3, get_check_type/1]).

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include_lib("datatypes/include/game.hrl").% -record(user,{})
-include("include/records.hrl").% -record(reg_info,{})
-include("include/command_parser.hrl").% User command keyword

%%------------------------------------------------------------------------------
%% @doc parse_login/1
%%
%% Parses a login string into a user record.
%% @end
%%------------------------------------------------------------------------------
parse_login(Data) ->
    RequiredFields = [?NICKNAME, ?PASSWORD],
    Values = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, Values) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Nick, Pw] = Values,

            case get_error_list(Values, get_check_type(RequiredFields),
                                RequiredFields) of
                [] ->
                    {ok, #user{nick = Nick, password = Pw}};
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
    end.


%%------------------------------------------------------------------------------
%% @doc parse_create/1
%%
%% Parses a create string into a game record.
%%
%% @end
%%------------------------------------------------------------------------------
parse_create(Data) ->
    RequiredFields = [?SESSION, ?GAMENAME, ?PRESSTYPE, ?ORDERCIRCLE,
                      ?RETREATCIRCLE, ?GAINLOSTCIRCLE, ?WAITTIME],
    OptionalFields = [?PASSWORD, ?DESCRIPTION, ?NUMBEROFPLAYERS],
    ReqValues = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Session, Name, Press, OrdPhase, RetPhase, BldPhase, WaitTime] = ReqValues,
            OptionalValues = get_required_fields(OptionalFields, Data),
            [Pw, Description, NumPlayers] = OptionalValues,

            case get_error_list(merge_list(ReqValues, OptionalValues),
                    get_check_type(merge_list(RequiredFields, OptionalFields)),
                                merge_list(RequiredFields, OptionalFields)) of
                [] ->
                    GameWithRequired = #game{
                        name = Name, press = Press,
                        order_phase = parse_time_format(OrdPhase),
                        retreat_phase = parse_time_format(RetPhase),
                        build_phase = parse_time_format(BldPhase),
                        waiting_time = parse_time_format(WaitTime),
                        creator_id = undefined},
                    PropList = [{#game.description, Description},
                                {#game.num_players, NumPlayers},
                                {#game.password, Pw}],
                    Game = proplist:update_record(GameWithRequired, PropList),
                    {ok, Session, Game};
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
    end.


%%------------------------------------------------------------------------------
%% @doc parse_reconfig/1
%%
%% Parses a reconfig string into a game proplist.
%%  Note: this function instead of record it return a prop list which each
%%   member of the is tuple of field name in game record and its value.
%%
%% @end
%%------------------------------------------------------------------------------
parse_reconfig(Data) ->
    RequiredFields = [?GAMEID, ?SESSION],
    OptionalFields = [?GAMENAME, ?PRESSTYPE, ?ORDERCIRCLE, ?RETREATCIRCLE,
                      ?GAINLOSTCIRCLE, ?WAITTIME, ?PASSWORD,
                      ?DESCRIPTION, ?NUMBEROFPLAYERS],
    ReqValues = get_required_fields(RequiredFields, Data),

    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [GameIdStr, Session] = ReqValues,
            OptionalValues = get_required_fields(OptionalFields, Data),
            [Name, Press, OrdPhase, RetPhase, BldPhase, WaitTime, Pw,
             Description, NumPlayers] = OptionalValues,
            case get_error_list(merge_list(ReqValues, OptionalValues),
                    get_check_type(merge_list(RequiredFields, OptionalFields)),
                                merge_list(RequiredFields, OptionalFields)) of
                [] ->
                    {ok, Session,
                     {list_to_integer(GameIdStr),
                      [{#game.name, Name}, {#game.press,  Press},
                       {#game.order_phase, parse_time_format(OrdPhase)},
                       {#game.retreat_phase, parse_time_format(RetPhase)},
                       {#game.build_phase, parse_time_format(BldPhase)},
                       {#game.waiting_time, parse_time_format(WaitTime)},
                       {#game.description, Description},
                       {#game.num_players, NumPlayers},
                       {#game.password, Pw},
                       {#game.creator_id, field_missing}]}};
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
    end.
%%------------------------------------------------------------------------------
%% @doc parse_register/1
%%
%% Parses a register string into a user record.
%%
%% @end
%%------------------------------------------------------------------------------
parse_register(Data) ->
    RequiredFields = [?NICKNAME, ?PASSWORD, ?EMAIL, ?FULLNAME],
    OptionalFields = [?CHANNEL],
    ReqValues = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Nick, Pw, Mail, Name] = ReqValues,
            [Channel] = get_required_fields(OptionalFields, Data),
            LowerChannel = case Channel of
                               field_missing ->
                                   undefined;
                               _ ->
                                   string:to_lower(Channel)
                           end,
            case get_error_list(merge_list(ReqValues, [LowerChannel]),
                    get_check_type(merge_list(RequiredFields, OptionalFields)),
                                merge_list(RequiredFields, OptionalFields)) of
                [] ->
                    {ok, #user{nick = Nick, password = Pw,
                               email = Mail, name = Name,
                               channel= case is_list(LowerChannel) of
                                            true -> list_to_atom(LowerChannel);
                                            false -> LowerChannel
                                        end}};
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
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
    RequiredFields = [?SESSION],
    OptionalFields = [?PASSWORD, ?EMAIL, ?FULLNAME],
    ReqValues = get_required_fields(RequiredFields, Data),

    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [Session] = ReqValues,
            OptionalValues = get_required_fields(OptionalFields, Data),
            [Pw, Mail, Name] = OptionalValues,

            case get_error_list(merge_list(ReqValues, OptionalValues),
                    get_check_type(merge_list(RequiredFields, OptionalFields)),
                                merge_list(RequiredFields, OptionalFields)) of
                [] ->
                    {ok, Session,
                     [{#user.password, Pw},
                      {#user.email, Mail},
                      {#user.name, Name}]};
                    % output user update proplist for optional fields
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc parse_overview/1
%%
%% Parses a game overview request
%%
%% @end
%%------------------------------------------------------------------------------
parse_overview(Data) ->
    RequiredFields = [?SESSION, ?GAMEID],
    ReqValues = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [SessionId, GameId] = ReqValues,

            case get_error_list(ReqValues,  get_check_type(RequiredFields),
                                RequiredFields) of
                [] ->
                    {ok, SessionId, list_to_integer(GameId)};
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc parse_join/1
%%
%% Parses a game join game request
%%
%% @end
%%------------------------------------------------------------------------------
parse_join(Data) ->
    RequiredFields = [?SESSION, ?GAMEID, ?COUNTRY],
    ReqValues = get_required_fields(RequiredFields, Data),
    case lists:member(field_missing, ReqValues) of
        true ->
            {error, {required_fields, RequiredFields}};
        false ->
            [SessionId, GameId, CountryStr] = ReqValues,

            case get_error_list(ReqValues,  get_check_type(RequiredFields),
                                RequiredFields) of
                [] ->
                    Country = list_to_atom(string:to_lower(CountryStr)),
                    case lists:member(Country, ?COUNTRIES) of
                        false -> {error, {invalid_input, CountryStr}};
                        true ->
                            {ok,
                             SessionId,
                             {list_to_integer(GameId),
                              Country}
                            }
                    end;
                ErrorList ->
                    {error, {invalid_input, ErrorList}}
            end
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
                              string:strip(Value, both);
                          nomatch ->
                              field_missing
                      end
              end, Fields).

%%------------------------------------------------------------------------------
%% @doc convert time format string to minutes
%%  Input:  "1D2H30M"
%%
%%  Output: (1)*60*24 + (2)*60 + (30)
%% @end
%%------------------------------------------------------------------------------
parse_time_format(field_missing) ->
    field_missing;
parse_time_format(TimeFormat) ->
    GetField = fun(Pattern) ->
        % Pattern must have a subpattern, e.g. "([0-9]+)D"
        case re:run(TimeFormat, Pattern, [{capture,[1],list}]) of
            {match, [Field]} ->
                list_to_integer(Field);
            nomatch ->
                0
        end
    end,
    Days = GetField("([0-9]+)D") * 1440,
    Hours = GetField("([0-9]+)H") * 60,
    Minutes = GetField("([0-9]+)M"),
    Days+Hours+Minutes.

%%------------------------------------------------------------------------------
%% @doc check if command values are valid
%% Input Arg1: (alpha_num_only|begin_with_alpha|num_only|alpha_only|mail_addr)
%%       Arg2: FieldValue
%% Output (true|false)
%% @end
%%------------------------------------------------------------------------------
is_valid_value(_, field_missing) -> true;
is_valid_value(_, undefined) -> true;
is_valid_value(FieldType, Value) ->
    Check = fun(IfAny ,Pattern) ->
        case re:run(Value, Pattern) of
            {match, _} ->
                case IfAny of
                    invalid_pattern ->
                        false;% invalid pattern detected, return false
                    valid_pattern ->
                        true% valid pattern detected, let it pass
                end;
            nomatch ->
                case IfAny of
                    invalid_pattern ->
                        true;% no invalid patterns found, let it pass
                    valid_pattern ->
                        false% no valid patterns found, return false
                end
        end
    end,
    case FieldType of
        alpha_num_only ->% only allow alphabets and number
            Check(invalid_pattern, "[^A-Za-z0-9_]");
        base64 ->
            Check(valid_pattern, "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$");
        begin_with_alpha ->% The first character must be alphabetic
            Check(valid_pattern, "(^[A-Za-z][A-Za-z0-9_]*$)");
        num_only ->% forbids non-number characters
            Check(invalid_pattern, "[^0-9]");
        alpha_space_only ->% forbids non-alphabetical characters
            Check(invalid_pattern, "[^A-Za-z\s]");
        mail_addr ->% check if is correct email format
            Check(valid_pattern, "^[0-9A-Za-z]+@[0-9A-Za-z]+\.[A-Za-z]+$");
        password ->
            Check(valid_pattern, "^[\s-~]+$");
        channel ->
            Check(valid_pattern, "\s*(mail|im|web)\s*");
        duration_time ->
            Check(valid_pattern, "^([0-9]+D)*([0-9]+H)*([0-9]+M)*$")
    end.


%%------------------------------------------------------------------------------
%% @doc return list of feild which failed the validation
%% Input
%%       Arg1: list of field values
%%       Arg2: list of input field types
%%       Arg3: list of input field names
%%  the size of the three input list should agree
%% Output [](if all input data is valid)|
%%        [term()]
%%        term():: string() which is the of invalid input fields
%% @end
%%------------------------------------------------------------------------------
get_error_list(ValueList, TypeList, TagList) ->
    CheckProplist = lists:zip3(ValueList, TypeList, TagList), %[{Value, CheckType, Tag}]
    get_error_list(CheckProplist, []).

get_error_list([], AccError) ->
    AccError;
get_error_list(TupleList, AccError) ->
    [{Value, FieldType, Tag}|Rest] = TupleList,
    case is_valid_value(FieldType, Value) of
        true ->
            get_error_list(Rest, AccError);
        false ->
            get_error_list(Rest, [Tag|AccError])
    end.

%%------------------------------------------------------------------------------
%% @doc it get list of input fields and return corresponding checking types
%% Input
%%       Arg1: list of input field names
%% Output [term()]
%%        term():: atom() which is the validation type checking class
%% @end
%%------------------------------------------------------------------------------
get_check_type(TagList) ->
    get_check_type(TagList,[]).

get_check_type([], AccCheckList) ->
    lists:reverse(AccCheckList);
get_check_type([H|Rest], AccCheckList) ->
    UpdatedCheckList = case H of
        ?SESSION -> [base64|AccCheckList];

        ?NICKNAME -> [begin_with_alpha|AccCheckList];
        ?PASSWORD -> [password|AccCheckList];
        ?FULLNAME -> [alpha_space_only|AccCheckList];
        ?EMAIL -> [mail_addr|AccCheckList];
        ?CHANNEL -> [channel|AccCheckList];

        ?DESCRIPTION -> [alpha_space_only|AccCheckList];
        ?GAMEID -> [num_only|AccCheckList];
        ?NUMBEROFPLAYERS -> [num_only|AccCheckList];

        ?GAMENAME -> [begin_with_alpha|AccCheckList];
        ?PRESSTYPE -> [alpha_space_only|AccCheckList];
        ?ORDERCIRCLE -> [duration_time|AccCheckList];
        ?RETREATCIRCLE -> [duration_time|AccCheckList];
        ?GAINLOSTCIRCLE -> [duration_time|AccCheckList];
        ?WAITTIME -> [duration_time|AccCheckList];

        ?COUNTRY -> [alpha_space_only|AccCheckList]
    end,
    get_check_type(Rest, UpdatedCheckList).



%%------------------------------------------------------------------------------
%% @doc attach two list of strings
%% Input
%%       Arg1: list of string
%%       Arg2: list of string
%%  the size of the input lists should agree
%% Output [term()]
%%        term():: string()
%% @end
%%------------------------------------------------------------------------------
merge_list(L1, L2) ->
    lists:foldl(fun(X, L) -> [X|L] end, L1, L2).
