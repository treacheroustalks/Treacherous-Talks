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
-spec
parse(Data::term()) ->
{register, {ok, #user{}}} |
{login, {ok, #user{}}} |
{update, {ok, string(), #user{}}} |
{create_game, {ok, string(), #game{}}} |
{reconfig_game, {ok, string(), #game{}}} |
{join_game, {ok, string(), {integer(), atom()}}} |
{game_overview, {ok, string(), integer()}} |
{game_order, {ok, string(), {integer(), term()}}} |
{games_current, {ok, string(), atom()}} |
{game_search, {ok, string(), string()}}.


parse(RawData) ->
    {Action, Data} = decode(RawData),
    case Action of
        "login" ->
            {login,
             {ok, #user{nick = get_field("nick", Data),
                        password = get_field("password", Data)}}};
        "get_session_user" ->
            {get_session_user, {ok, get_field("session_id", Data), dummy}};
        "register" ->
            {register,
             {ok, #user{nick = get_field("nick", Data),
                        password = get_field("password", Data),
                        email = get_field("email", Data),
                        name = get_field("name", Data)}}};
        "update_user" ->
            {update_user,
             {ok,
              get_field("session_id", Data),
              [{#user.password, get_field("password", Data)},
               {#user.email, get_field("email", Data)},
               {#user.name, get_field("name", Data)}]}};
        "get_game" ->
            GameId = get_integer("game_id", Data),
            {get_game, {ok, get_field("session_id", Data), GameId}};
        "create_game" ->
            OrderPhase = get_integer("order_phase", Data),
            RetreatPhase = get_integer("retreat_phase", Data),
            BuildPhase = get_integer("build_phase", Data),
            WaitingTime = get_integer("waiting_time", Data),
            NumPlayers = get_integer("num_players", Data),
            {create_game,
             {ok,
              get_field("session_id", Data),
              #game{name = get_field("name", Data),
                    description = get_field("description", Data),
                    press = get_field("press", Data),
                    password = get_field("password", Data),
                    order_phase = OrderPhase,
                    retreat_phase = RetreatPhase,
                    build_phase = BuildPhase,
                    waiting_time = WaitingTime,
                    num_players = NumPlayers,
                    creator_id = undefined}}};
        "reconfig_game" ->
            GameId = get_integer("game_id", Data),
            OrderPhase = get_integer("order_phase", Data),
            RetreatPhase = get_integer("retreat_phase", Data),
            BuildPhase = get_integer("build_phase", Data),
            WaitingTime = get_integer("waiting_time", Data),
            NumPlayers = get_integer("num_players", Data),
            {reconfig_game,
             {ok,
              get_field("session_id", Data),
              {GameId,
              [{#game.name, get_field("name", Data)},
               {#game.press,  get_field("press", Data)},
               {#game.order_phase, OrderPhase},
               {#game.retreat_phase, RetreatPhase},
               {#game.build_phase, BuildPhase},
               {#game.waiting_time, WaitingTime},
               {#game.description, get_field("description", Data)},
               {#game.num_players, NumPlayers},
               {#game.password, get_field("password", Data)},
               {#game.creator_id, field_missing}]}}};
        "join_game" ->
            GameId = get_integer("game_id", Data),
            Country = list_to_atom(get_field("country", Data)),
            {join_game, {ok, get_field("session_id", Data), {GameId, Country}}};
        "game_overview" ->
            GameId = get_integer("game_id", Data),
            {game_overview, {ok, get_field("session_id", Data), GameId}};
        "game_order" ->
            GameId = get_integer("game_id", Data),
            OrderLines = string:tokens(get_field("game_order", Data), "\r\n,"),
            GameOrders = player_orders:interpret_str_orders(OrderLines),
            % Remove error orders
            ResultOrders = lists:filter(fun(X)-> element(1, X) /= error end,
                                           GameOrders),
            {game_order, {ok, get_field("session_id", Data),
                          {GameId, ResultOrders}}};
        "games_current" ->
            {games_current, {ok, get_field("session_id", Data), dummy}};
        "game_search" ->
            Query = get_search_query(Data),
            {game_search, {ok, get_field("session_id", Data), Query}}
    end.


%% Get a specific field from a list of tuples and convert to integer if it isn't
get_integer(Key, Data) ->
    Value = get_field(Key, Data),
    case is_integer(Value) of
        true ->
            Value;
        false ->
            list_to_integer(Value)
    end.

%% Get a specific field from a list of tuples
get_field(Key, Data) ->
    {Key, Value} = lists:keyfind(Key, 1, Data),
    case Value of
        "" ->
            "";
        _ ->
            Value
    end.

%% Generate search query from input data
get_search_query(Data) ->
    Params = ["name", "description", "press", "status", "order_phase",
              "retreat_phase", "build_phase", "waiting_time", "num_players"],
    lists:foldl(fun(Key, Query) ->
                        Val = get_field(Key, Data),
                        Value = to_string(Val),
                        case {Query, Val} of
                            {_, ""} ->
                                Query;
                            {"", _} ->
                                get_query_string(Key, Value);
                            _ ->
                                Query ++ " AND " ++ get_query_string(Key, Value)
                        end
                end, "", Params).

to_string(Val) ->
    case data_format:type_of(Val) of
        list -> Val;
        integer -> integer_to_list(Val);
        atom -> atom_to_list(Val)
    end.

get_query_string(Key, Value) ->
    Key ++ "=" ++ Value.

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