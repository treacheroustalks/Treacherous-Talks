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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%% @author Tiina Loukusa <loukusa@gmail.com>
%%%
%%% @doc Load test functions.
%%%
%%% @since : 21 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(load_test).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------
-export([
         register_user/0, register_user/1,
         login/2, login/1,
         logout/2,
         send_msg/2, send_game_msg/3,
         read_push_msg/1,
         create_game/1, create_game/2,
         join_game/3, join_full_game/2,
         start_game/2, start_games/2,
         phase_change/2,
         game_overview/2,
         create_orders/0, send_order/4, send_orders/4,
         search_current/1,
         get_all_countries/0,
         game_message_receiver/1, game_multiple_message_receiver/1,
         user_message_receiver/1,
         get_user_pids/1, get_user_sessions/1,
         get_phase_years/1
        ]).

% Number of times we poll message inbox
-define(MESSAGE_COUNT, 100).
% Time we wait before we poll the message inbox (in milliseconds)
-define(MESSAGE_SLEEP, 10).

%% ------------------------------------------------------------------
%% exports for eUnit only
%% ------------------------------------------------------------------
-export([
         order_transform/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("utils/include/player_orders.hrl").

-include_lib("datatypes/include/push_receiver.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").


-record(state, {msg = [], game_msg = []}).

%% ------------------------------------------------------------------
%% API implementation
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Register a new user
%% ------------------------------------------------------------------
-spec register_user() -> {Nick::string(), Password::string()}.
register_user() ->
    Nick = "load_test_" ++ integer_to_list(random()),
    User = create_user(Nick),
    Cmd = {register, {ok, User}},
    case controller:handle_action(Cmd, callback()) of
        {{register, success}, _} ->
            {Nick, User#user.password};
        Error ->
            erlang:error(Error)
    end.

%% ------------------------------------------------------------------
%% Register multiple users
%% ------------------------------------------------------------------
-spec register_user(integer()) -> [{Nick::string(), Password::string()}].
register_user(Count) ->
    lists:map(fun(_Num) -> register_user() end, lists:seq(1, Count)).

%% ------------------------------------------------------------------
%% Login user
%% ------------------------------------------------------------------
-spec login(Nick::string(), Password::string()) ->
          {SessionId::string(), Pid::pid()}.
login(Nick, Password) ->
    Pid = spawn(fun() ->push_receiver(#state{}) end),
    Receiver = #push_receiver{
                              pid = Pid,
                              args = no_args,
                              type = default},
    Cmd = {login, {ok, {#user{nick = Nick, password = Password},
                        Receiver}}},
    {{login, success}, SessionId} = controller:handle_action(Cmd, callback()),
    {SessionId, Pid}.

%% ------------------------------------------------------------------
%% Login multiple users
%% ------------------------------------------------------------------
-spec login([{Nick::string(), Password::string()}]) ->
          [{SessionId::string(), Pid::pid()}].
login(Users) ->
    lists:map(fun({Nick, Password}) -> login(Nick, Password) end, Users).

%% ------------------------------------------------------------------
%% Logout given user
%% ------------------------------------------------------------------
%@todo -spec logout(SessionId::string(), Pid::pid()) -> {[#message{}], [#game_message{}]}.
logout(SessionId, Pid) ->
    Cmd = {logout, {ok, SessionId, []}},
    controller:handle_action(Cmd, callback()),

    ReturnVal = read_push_msg(Pid),
    Pid ! stop,
    ReturnVal.

%% ------------------------------------------------------------------
%% Send message to a user
%% ------------------------------------------------------------------
-spec send_msg(SessionId::string(), Nick::string()) -> ok.
send_msg(SessionId, Nick) ->
    Msg = create_msg(Nick),
    Cmd = {user_msg, {ok, SessionId, Msg}},
    {{user_msg, success}, _} = controller:handle_action(Cmd, callback()),
    ok.

%% ------------------------------------------------------------------
%% Read pushed messages
%% ------------------------------------------------------------------
-spec read_push_msg(Pid::pid()) -> {[#message{}], [#game_message{}]}.
read_push_msg(Pid) ->
    Pid ! {read_msg, self()},
    receive
        Data ->
            Data
    end.

%% ------------------------------------------------------------------
%% Create game
%% ------------------------------------------------------------------
-spec create_game(SessionId::string()) -> GameId::integer().
create_game(SessionId) ->
    Game = create_game(),
    Cmd = {create_game, {ok, SessionId, Game}},
    {{create_game, success}, GameId} =
        controller:handle_action(Cmd, callback()),
    GameId.

%% ------------------------------------------------------------------
%% Create multiple games
%% ------------------------------------------------------------------
-spec create_game(SessionId::string(), Count::integer()) -> [GameId::integer()].
create_game(SessionId, Count) ->
    lists:map(fun(_Num) ->
                      create_game(SessionId)
              end, lists:seq(1, Count)).

%% ------------------------------------------------------------------
%% Join a user to the given game
%% ------------------------------------------------------------------
-spec join_game(SessionId::string(), GameId::integer(), Country::country()) ->
          ok | any().
join_game(SessionId, GameId, Country) ->
    Cmd = {join_game, {ok, SessionId, {GameId, Country}}},
    case controller:handle_action(Cmd, callback()) of
        {{join_game, success}, _} ->
            ok;
        {{join_game, invalid_data}, Error}->
            Error
    end.

%% ------------------------------------------------------------------
%% Join given users to the game
%% ------------------------------------------------------------------
-spec join_full_game([SessionId::string()], GameId::integer()) ->
          [{SessionId::string(), Country::atom()}].
join_full_game(Sessions, GameId) ->
    GameUsers = lists:zip(Sessions, get_all_countries()),
    lists:foreach(fun({SessionId, Country}) ->
                          join_game(SessionId, GameId, Country)
                  end, GameUsers),
    GameUsers.

%% ------------------------------------------------------------------
%% Move given game to the next phase
%% ------------------------------------------------------------------
-spec phase_change(Node::string(), GameId::integer()) -> ok.
phase_change(Node, GameId) ->
    rpc:call(Node, game_timer, sync_event, [GameId, timeout]).

%% ------------------------------------------------------------------
%% Trigger the start of the game
%% ------------------------------------------------------------------
-spec start_game(Node::string(), GameId::integer()) -> ok.
start_game(Node, GameId) ->
    phase_change(Node, GameId).

%% ------------------------------------------------------------------
%% Trigger the start of multiple games
%% ------------------------------------------------------------------
-spec start_games(Node::string(), [GameId::integer()]) -> ok.
start_games(Node, Games) ->
    lists:foreach(fun(GameId) -> start_game(Node, GameId) end, Games).

%% ------------------------------------------------------------------
%% Get the overview of the game
%% ------------------------------------------------------------------
-spec game_overview(SessionId::string(),
                    GameId::integer()) -> #game_overview{}.
game_overview(SessionId, GameId) ->
    Cmd = {game_overview, {ok, SessionId, GameId}},
    {{game_overview, success},
     GameOverview} = controller:handle_action(Cmd, callback()),
    GameOverview.

%% ------------------------------------------------------------------
%% Get orders from gen_moves
%% ------------------------------------------------------------------
-spec create_orders() -> [dict()].
create_orders() ->
    Orders = gen_moves:generate_orders(map_data:create(standard_game)),
    lists:map(fun(Dict) ->
                      countries_transform(get_all_countries(), Dict)
              end, Orders).

%% ------------------------------------------------------------------
%% Send orders for one country
%% ------------------------------------------------------------------
-spec send_order(GameId::integer(),
                 Orders::[dict()],
                 {Phase::atom(), Season::atom(), Year::integer()},
                 {SessionId::string(), Country::country()}) -> ok.
send_order(GameId, Orders, {Phase, Season, Year}, {SessionId, Country}) ->
    [OrderDict] = lists:filter(fun(Dict) ->
                                       {ok, Phase2} = dict:find(phase, Dict),
                                       {ok, Season2} = dict:find(season, Dict),
                                       {ok, Year2} = dict:find(year, Dict),
                                       if
                                           Phase == Phase2,
                                           Season == Season2,
                                           Year == Year2 ->
                                               true;
                                           true ->
                                               false
                                       end
                               end, Orders),
    {ok, CountryOrders} = dict:find({orders, Country}, OrderDict),
    Cmd = {game_order, {ok, SessionId, {GameId, CountryOrders}}},
    controller:handle_action(Cmd, callback()),
    ok.

%% ------------------------------------------------------------------
%% Send orders for many countries
%% ------------------------------------------------------------------
-spec send_orders(GameId::integer(),
                 Orders::[dict()],
                 Phase::{atom(), atom(), integer()},
                 [{SessionId::string(), Country::country()}]) -> ok.
send_orders(GameId, Orders, Phase, GameUsers) ->
    lists:foreach(fun(GameUser) ->
                          load_test:send_orders(GameId, Orders, Phase, GameUser)
                  end, GameUsers).

%% ------------------------------------------------------------------
%% Send game message
%% ------------------------------------------------------------------
-spec send_game_msg(SessionId::string(),
                    GameID::integer(),
                    Countries::[country()]) -> ok.
send_game_msg(SessionId, GameID, Countries) ->
    Msg = create_game_msg(GameID, Countries),
    Cmd = {game_msg, {ok, SessionId, Msg}},
    {{game_msg, success}, _} = controller:handle_action(Cmd, callback()),
    ok.

%% ------------------------------------------------------------------
%% Get current games of a user via search
%% ------------------------------------------------------------------
-spec search_current(SessionId::string()) -> [#game{}].
search_current(SessionId) ->
    Cmd = {games_current, {ok, SessionId, no_arg}},
    {{games_current, success}, Games} = controller:handle_action(Cmd, callback()),
    Games.

%% ------------------------------------------------------------------
%% Check user's inbox for game messages
%% ------------------------------------------------------------------
-spec game_message_receiver(Pid::pid()) ->
          {ok, success} | {error, Error::string()}.
game_message_receiver(Pid) ->
    message_receiver({0, Pid, game}).

%% ------------------------------------------------------------------
%% Check multiple users' inbox for game messages
%% ------------------------------------------------------------------
-spec game_multiple_message_receiver(Pids::[pid()]) ->
          {ok, success} | {error, Error::string()}.
game_multiple_message_receiver([]) ->
    {ok, success};
game_multiple_message_receiver([Pid|Pids]) ->
    case message_receiver({0, Pid, game}) of
        {ok, _} ->
            game_multiple_message_receiver(Pids);
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% Check user's inbox for user messages
%% ------------------------------------------------------------------
-spec user_message_receiver(Pid::pid()) ->
          {ok, success} | {error, Error::string()}.
user_message_receiver(Pid) ->
    message_receiver({0, Pid, user}).

%% ------------------------------------------------------------------
%% Get only session ids for users from data returned from register_user
%% ------------------------------------------------------------------
-spec get_user_sessions(Users::[{SessionId::string(), Pid::pid()}]) ->
          [SessionId::string()].
get_user_sessions(Users) ->
    [SessionId ||{SessionId, _Pid} <- Users].

%% ------------------------------------------------------------------
%% Get only pids for users from data returned from register_user
%% ------------------------------------------------------------------
-spec get_user_pids(Users::[{SessionId::string(), Pid::pid()}]) ->
          [Pid::pid()].
get_user_pids(Users) ->
    [Pid ||{_SessionId, Pid} <- Users].

%% ------------------------------------------------------------------
%% Get phases for specified number of years
%% ------------------------------------------------------------------
-spec get_phase_years(Count::integer()) -> [{atom(), atom(), string()}].
get_phase_years(Count) ->
    Data = lists:map(fun(Num) ->
                             Year = 1900 + Num,
                             get_phase_year(Year)
                     end, lists:seq(1, Count)),
    lists:flatten(Data).

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------
random() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]).

create_user(Nick) ->
    #user{id = undefined,
          nick = Nick,
          email = "test@user.com",
          password = "test_passw0rd",
          name = "Test User",
          role = user,
          channel = mail,
          last_ip = {127, 0, 0, 0},
          last_login = never,
          score = 0,
          date_created = {{2011, 10, 18}, {10, 42, 15}},
          date_updated = {{2011, 10, 18}, {10, 42, 16}}}.

create_game() ->
    #game{name="game name" ++ integer_to_list(random()),
          description="lorem ipsum dolor sit amet",
          press = white,
          order_phase = 1,
          retreat_phase = 1,
          build_phase = 1,
          password="",
          waiting_time = 60,
          num_players = 7
         }.

create_msg(Nick) ->
    #frontend_msg{to = Nick,
                  content = "Hello, some message!"}.

create_game_msg(GameId, Countries) ->
    #frontend_msg{to = Countries,
                  game_id = GameId,
                  content = "Hello, some message!"}.

callback(_Args, Result, Info) ->
    {Result, Info}.
callback() ->
    {fun callback/3, []}.

order_transform(Order) ->
    case Order of
        {move, {Unit, _Country}, From, To} ->
            #move{subj_unit = Unit, subj_src_loc = From,
                  subj_dst_loc = To};
        {hold, {Unit, _Country}, Loc} ->
            #hold{subj_unit = Unit, subj_loc = Loc};
        {support, {SupUnit, _}, SupLoc,
         {move, {Unit, _}, From, To}} ->
            #support_move{subj_unit = SupUnit,
                          subj_loc = SupLoc,
                          obj_unit = Unit,
                          obj_src_loc = From,
                          obj_dst_loc = To};
        {support, {SupUnit, _}, SupLoc,
         {hold, {Unit, _}, Loc}} ->
            #support_hold{subj_unit = SupUnit,
                          subj_loc = SupLoc,
                          obj_unit = Unit,
                          obj_loc = Loc};
        {convoy, {Fleet, _}, Loc,
         {Army, _}, From, To}->
            #convoy{subj_unit = Fleet,
                    subj_loc = Loc,
                    obj_unit = Army,
                    obj_src_loc = From,
                    obj_dst_loc = To};
        {build, {Unit, _}, Loc} ->
            #build{obj_unit = Unit, obj_loc = Loc};
        {disband, {Unit, _}, Loc} ->
            #disband{subj_unit = Unit, subj_loc = Loc}
    end.

%% ------------------------------------------------------------------
%% Receiver process
%% ------------------------------------------------------------------
push_receiver(State) ->
    receive
        {push, _Args, #push_event{
                                  type = off_game_msg,
                                  data = Msg}} ->
            OldMsg = State#state.msg,
            push_receiver(State#state{msg = [Msg | OldMsg]});
        {push, _Args, #push_event{
                                  type = in_game_msg,
                                  data = Msg}} ->
            OldMsg = State#state.game_msg,
            push_receiver(State#state{game_msg = [Msg | OldMsg]});
        {read_msg, Pid} ->
            Msg = State#state.msg,
            GameMsg = State#state.game_msg,
            Pid ! {Msg, GameMsg},
            push_receiver(#state{});
        stop ->
            ok;
        _ ->
            push_receiver(State)
    end.

message_receiver({Count, Pid, Type}) ->
    receive
        after 0 ->
            {Messages, GameMessages} = read_push_msg(Pid),
            Size = case Type of
                       game ->
                           length(GameMessages);
                       user ->
                           length(Messages)
                   end,
            case Size of
                0 ->
                    case Count > ?MESSAGE_COUNT of
                        true ->
                            {error, message_not_received};
                        false ->
                            timer:sleep(?MESSAGE_SLEEP),
                            message_receiver({Count+1, Pid, Type})
                    end;
                1 ->
                    {ok, success};
                _ ->
                    {error, magic_message}
            end
    end.

get_all_countries() ->
    [england, germany, france, italy, russia, turkey, austria].

countries_transform([], Dict) ->
    Dict;
countries_transform([Country|Countries], Dict) ->
    {ok, Ord} = dict:find({orders, Country}, Dict),
    NewOrd = lists:map(fun order_transform/1, Ord),
    NewDict = dict:store({orders, Country}, NewOrd, Dict),
    countries_transform(Countries, NewDict).

get_phase_year(Year) ->
    [{order_phase, spring, Year},
     {retreat_phase, spring, Year},
     {order_phase, fall, Year},
     {retreat_phase, fall, Year},
     {build_phase, fall, Year}].