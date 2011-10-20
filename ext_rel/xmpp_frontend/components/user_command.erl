%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @module user_command @end
%%%
%%% @doc A module for recognizing user command in email body
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(user_command).

%Export for API
-export([get_reg_info/1,
     get_reg_info_xmpp/1, new_user_record/1,new_user_record/2]).

%Export for eunit test
-export([reg_info_refine/1]).

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include_lib("user_command.hrl").% -record(reg_info,{})

%%------------------------------------------------------------------------------------------------
%% @function get_reg_info/1 @end
%%
%% @doc Convert reg_info record to user record.
%%------------------------------------------------------------------------------------------------
get_reg_info(BinStr) ->
    HeadPos = binary:match(BinStr, <<"REGISTER\r\n">>),
    case HeadPos of
        {_,_} ->
            HeadCut = bin_utils:tailstr(BinStr, HeadPos),
            TailPos = binary:match(HeadCut, <<"\r\nEND">>),
            case TailPos of
                {_,_} ->
                    TailCut = bin_utils:headstr(HeadCut, TailPos),
                    RawInfoList = binary:split(TailCut, <<"\r\n">>, [global, trim]),
                    reg_info_refine(RawInfoList);
                _ -> {error, no_reg_end}
            end;
        _ ->
            {error, no_reg_start}
    end.

get_reg_info_xmpp(
  BinStr) ->
    HeadPos = binary:match(BinStr, <<"REGISTER,">>),
    case HeadPos of
        {_,_} ->
            HeadCut = bin_utils:tailstr(BinStr, HeadPos),
            TailPos = binary:match(HeadCut, <<"END">>),
            case TailPos of
                {_,_} ->
                    TailCut = bin_utils:headstr(HeadCut, TailPos),
                    RawInfoList = binary:split(TailCut, <<",">>, [global, trim]),
                    reg_info_refine(RawInfoList);
                _ -> {error, no_reg_end}
            end;
        _ ->
            {error, no_reg_start}
    end.


%%------------------------------------------------------------------------------------------------
%% @function reg_info_refine/1 @end
%%
%% @doc Convert reg_info record to user record.
%%------------------------------------------------------------------------------------------------
reg_info_refine([], OutputRecord) ->
    {ok, OutputRecord};
reg_info_refine(
  [H|Rest],
  OutputRecord) ->
    case binary:split(H, <<":">>) of
        [Field, Value]
          when H =/= <<>> -> % if current line has ":", proceed further syntax analysis
            Field1 = bin_utils:strip(Field),
            Value1 = bin_utils:strip(Value),

            case Field1 of % check of valid field type
                <<"NICKNAME">> ->
                    reg_info_refine(Rest, #reg_info{
                        nick = Value1,
                        password = OutputRecord#reg_info.password,
                        email = OutputRecord#reg_info.email,
                        name = OutputRecord#reg_info.name
                    });
                <<"PASSWORD">> ->
                    reg_info_refine(Rest, #reg_info{
                        nick = OutputRecord#reg_info.nick,
                        password = Value1,
                        email = OutputRecord#reg_info.email,
                        name = OutputRecord#reg_info.name
                    });
                <<"EMAIL">> ->
                    case binary:match(Value1, <<"@">>) of
                        {_,_} ->
                            reg_info_refine(Rest, #reg_info{
                                nick = OutputRecord#reg_info.nick,
                                password = OutputRecord#reg_info.password,
                                email = Value1,
                                name = OutputRecord#reg_info.name
                            });
                        _ ->
                            {error, invalid_email_address}
                    end;
                <<"FULLNAME">> ->
                    reg_info_refine(Rest, #reg_info{
                        nick = OutputRecord#reg_info.nick,
                        password = OutputRecord#reg_info.password,
                        email = OutputRecord#reg_info.email,
                        name = Value1
                    });
                _ ->
                    reg_info_refine(Rest, OutputRecord)
            end;
        _ -> % if current line doen't have ":", skip this line
            reg_info_refine(Rest, OutputRecord)
    end.
reg_info_refine(
  InfoList) ->
    reg_info_refine(InfoList, #reg_info{}).


%%------------------------------------------------------------------------------------------------
%% @function new_user_record/1 @end
%%
%% @doc Convert reg_info record to user record.
%%------------------------------------------------------------------------------------------------
new_user_record(RegInfo) ->
    #user{
        nick     = RegInfo#reg_info.nick,
        password = RegInfo#reg_info.password,
        email    = RegInfo#reg_info.email,
        name     = RegInfo#reg_info.name,
    channel  = smtp
    }.

new_user_record(RegInfo, Ip) ->
    #user{
        nick     = RegInfo#reg_info.nick,
        password = RegInfo#reg_info.password,
        email    = RegInfo#reg_info.email,
        name     = RegInfo#reg_info.name,
    last_ip  = Ip,
    channel  = xmpp
    }.
