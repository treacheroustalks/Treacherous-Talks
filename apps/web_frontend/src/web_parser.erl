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
            GameId = list_to_integer(get_field("game_id", Data)),
            {get_game, {ok, get_field("session_id", Data), GameId}};
        "create_game" ->
            OrderPhase = list_to_integer(get_field("order_phase", Data)),
            RetreatPhase = list_to_integer(get_field("retreat_phase", Data)),
            BuildPhase = list_to_integer(get_field("build_phase", Data)),
            WaitingTime = list_to_integer(get_field("waiting_time", Data)),
            NumPlayers = list_to_integer(get_field("num_players", Data)),
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
            GameId = list_to_integer(get_field("game_id", Data)),
            OrderPhase = list_to_integer(get_field("order_phase", Data)),
            RetreatPhase = list_to_integer(get_field("retreat_phase", Data)),
            BuildPhase = list_to_integer(get_field("build_phase", Data)),
            WaitingTime = list_to_integer(get_field("waiting_time", Data)),
            NumPlayers = list_to_integer(get_field("num_players", Data)),
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
            GameId = list_to_integer(get_field("game_id", Data)),
            Country = list_to_atom(get_field("country", Data)),
            {join_game, {ok, get_field("session_id", Data), {GameId, Country}}};
        "game_overview" ->
            GameId = list_to_integer(get_field("game_id", Data)),
            {game_overview, {ok, get_field("session_id", Data), GameId}}
    end.

%% Get a specific field from a list of tuples
get_field(Key, Data) ->
    {Key, Value} = lists:keyfind(Key, 1, Data),
    case Value of
        "" ->
            undefined;
        _ ->
            Value
    end.

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