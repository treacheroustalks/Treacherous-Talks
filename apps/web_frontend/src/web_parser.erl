%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Module parses data sent by the user
%%% @end
%%%
%%% @since : 26 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(web_parser).
-export([parse/1]).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Web parser function
%% Expects two arguments
%% Action - chooses the message that has to be generated
%% Data - Data in the form of Erlang version of JSON that is yet to be decoded.
%%        Data is used to create the message content based in Action
%% @end
%% [@spec
%% parse(Action::string(), Data::term()) ->
%% {register, {ok, #user{}}} |
%% {login, {ok, #user{}}} |
%% {update, {ok, SessionId, #user{}}} |
%% {create_game, {ok, SessionId, #game{}}}
%% @end]
%%-------------------------------------------------------------------
parse(RawData) ->
    {Action, Data} = decode(RawData),
    case Action of
        "login" ->
            {login,
             {ok, #user{nick = get_field("nick", Data),
                        password = get_field("password", Data)}}};
        "register" ->
            {register,
             {ok, #user{nick = get_field("nick", Data),
                        password = get_field("password", Data),
                        email = get_field("email", Data),
                        name = get_field("fullname", Data)}}};
        "create_game" ->
            SessionId = list_to_integer(get_field("session_id", Data)),
            {create_game,
             {ok,
              SessionId,
              #game{name = get_field("name", Data),
                    press = get_field("press", Data),
                    order_phase =
                        list_to_integer(get_field("order_phase", Data)),
                    retreat_phase =
                        list_to_integer(get_field("retreat_phase", Data)),
                    build_phase =
                        list_to_integer(get_field("build_phase", Data)),
                    waiting_time =
                        list_to_integer(get_field("waiting_time", Data)),
                    creator_id = undefined}}};
        "update_user" ->
            SessionId = list_to_integer(get_field("session_id", Data)),
            {update_user,
             {ok,
              SessionId,
              [{#user.password, get_field("password", Data)},
               {#user.email, get_field("email", Data)},
               {#user.name, get_field("fullname", Data)}]}};
        "get_session_user" ->
            SessionId = list_to_integer(get_field("session_id", Data)),
            {get_session_user, {ok, SessionId}}
    end.

%% Get a specific field from a list of tuples
get_field(Key, Data) ->
    {Key, Value} = lists:keyfind(Key, 1, Data),
    Value.

%% Convert JSON decoded erlang into list of tuples
decode(RawData) ->
    case RawData of
        {ok, {struct, RawData1}} ->
            [{"action", Action}, {"data", RawData2}] = RawData1,
            {array, RawData3} = RawData2,
            {Action, remove_struct(RawData3)};
        _ ->
            error
    end.

remove_struct(L) ->
    remove_struct(L, []).

remove_struct([], Acc) ->
    Acc;
remove_struct([{struct, [Elem]}|T], Acc) ->
    remove_struct(T, [Elem|Acc]).