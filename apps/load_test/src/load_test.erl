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
         register_user/0,
         register_user/1,
         login/2,
         login/1,
         logout/2,
         send_msg/2,
         read_push_msg/1,
         create_game/1,
         create_game/2,
         join_game/3,
         join_full_game/2,
         game_overview/2,
         create_orders/0,
         send_orders/7,
         send_game_msg/3,
         search_current/1,
         start_game/2,
         start_games/2,
         phase_change/2
        ]).

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
-spec register_user() -> {Nick::string(), Password::string()}.
register_user() ->
    Nick = "load_test_" ++ integer_to_list(random()),
    User = create_user(Nick),
    Cmd = {register_user, {ok, User}},
    controller:handle_action(Cmd, callback()),
    {Nick, User#user.password}.

-spec register_user(integer()) -> [{Nick::string(), Password::string()}].
register_user(Count) ->
    register_user(Count, []).

register_user(0, Users) ->
    Users;
register_user(Count, Users) ->
    User = register_user(),
    register_user(Count-1, [User|Users]).

-spec login(Nick::string(), Password::string()) ->
                   {SessionId::string(), Pid::pid()}.
login(Nick, Password) ->
    Pid = spawn(fun() ->receiver(#state{}) end),
    Receiver = #push_receiver{
      pid = Pid,
      args = no_args,
      type = default},
    Cmd = {login, {ok, {#user{nick = Nick, password = Password},
                        Receiver}}},
    {{login, success}, SessionId} = controller:handle_action(Cmd, callback()),
    {SessionId, Pid}.

-spec login([{Nick::string(), Password::string()}]) ->
          [{SessionId::string(), Pid::pid()}].
login(Users) ->
    lists:map(fun({Nick, Password}) -> login(Nick, Password) end, Users).

%@todo -spec logout(SessionId::string(), Pid::pid()) -> {[#message{}], [#game_message{}]}.
logout(SessionId, Pid) ->
    Cmd = {logout, {ok, SessionId, []}},
    controller:handle_action(Cmd, callback()),

    ReturnVal = read_push_msg(Pid),
    Pid ! stop,
    ReturnVal.

-spec send_msg(SessionId::string(), Nick::string()) -> ok.
send_msg(SessionId, Nick) ->
    Msg = create_msg(Nick),
    Cmd = {user_msg, {ok, SessionId, Msg}},
    {{user_msg, success}, _} = controller:handle_action(Cmd, callback()),
    ok.

% @todo -spec read_push_msg(Pid::pid()) -> {[#message{}], [#game_message{}]}.
read_push_msg(Pid) ->
    Pid ! {read_msg, self()}.

-spec create_game(SessionId::string()) -> GameId::integer().
create_game(SessionId) ->
    Game = create_game(),
    Cmd = {create_game, {ok, SessionId, Game}},
    {{create_game, success}, GameId} =
        controller:handle_action(Cmd, callback()),
    GameId.

-spec create_game(SessionId::string(), Count::integer()) -> [GameId::integer()].
create_game(SessionId, Count) ->
    create_game(SessionId, Count, []).

create_game(_SessionId, 0, Games) ->
    Games;
create_game(SessionId, Count, Games) ->
    Game = create_game(SessionId),
    create_game(SessionId, Count-1, [Game|Games]).

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

-spec join_full_game([SessionId::string()], GameId::integer()) ->
                       ok | any().
join_full_game(Sessions, GameId) ->
    GU = lists:zip(Sessions, get_all_countries()),
    [join_game(SessionId, GameId, Country) || {SessionId, Country} <- GU],
    ok.

-spec phase_change(Node::string(), GameId::integer()) -> ok.
phase_change(Node, GameId) ->
    rpc:call(Node, game_timer, sync_event, [GameId, timeout]),
    ok.

-spec start_game(Node::string(), GameId::integer()) -> ok.
start_game(Node, GameId) ->
    phase_change(Node, GameId).

-spec start_games(Node::string(), [GameId::integer()]) -> ok.
start_games(Node, Games) ->
    lists:foreach(fun(GameId) -> phase_change(Node, GameId) end, Games).

-spec game_overview(SessionId::string(),
                    GameId::integer()) -> #game_overview{}.
game_overview(SessionId, GameId) ->
    Cmd = {game_overview, {ok, SessionId, GameId}},
    {{game_overview, success}, GameOverview} = controller:handle_action(Cmd, callback()),
    GameOverview.

-spec create_orders() -> [dict()].
create_orders() ->
    Orders = gen_moves:generate_orders(map_data:create(standard_game)),
    Countries = get_all_countries(),
    CountryTransform = fun(Dict, Country) ->
                               {ok, Ord} = dict:find({orders, Country}, Dict),
                               NewOrd = lists:map(fun order_transform/1, Ord),
                               dict:store({orders, Country}, NewOrd, Dict)
                       end,
    lists:map(fun(Dict) ->
                      lists:map(fun(Country) ->
                                        CountryTransform(Dict, Country)
                                end, Countries)
              end, Orders).

-spec send_orders(Orders::[dict()],
                  Phase::atom(),
                  Season::atom(),
                  Year::integer(),
                  SessionId::string(),
                  GameId::integer(),
                  Country::country()) -> ok.
send_orders(Orders, Phase, Season, Year, SessionId, GameId, Country) ->
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
    {ok, Orders} = dict:find({orders, Country}, OrderDict),
    ?debugVal(Orders),
    Cmd = {game_order, {ok, SessionId, {GameId, Orders}}},
    controller:handle_action(Cmd, callback()),
    ok.

-spec send_game_msg(SessionId::string(),
                    GameID::integer(),
                    Countries::[country()]) -> ok.
send_game_msg(SessionId, GameID, Countries) ->
    Msg = create_game_msg(GameID, Countries),
    Cmd = {game_msg, {ok, SessionId, Msg}},
    {{game_msg, success}, _} = controller:handle_action(Cmd, callback()),
    ok.

-spec search_current(SessionId::string()) -> [#game{}].
search_current(SessionId) ->
    Cmd = {games_current, {ok, SessionId, no_arg}},
    {{games_current, success}, Games} = controller:handle_action(Cmd, callback()),
    Games.


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
          waiting_time = 1,
          num_players = 7
         }.

create_msg(Nick) ->
      #frontend_msg{to = Nick,
                    content = "Hello, some message!"}.

create_game_msg(_GameId, Countries) ->
      #frontend_msg{to = Countries,
% @todo                    game_id = GameId,
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
receiver(State) ->
    receive
        {push, _Args, #push_event{
                 type = off_game_msg,
                 data = Msg}} ->
            OldMsg = State#state.msg,
            receiver(State#state{msg = [Msg | OldMsg]});
        {push, _Args, #push_event{
                 type = in_game_msg,
                 data = Msg}} ->
            OldMsg = State#state.game_msg,
            receiver(State#state{game_msg = [Msg | OldMsg]});
        {read_msg, Pid} ->
            Msg = State#state.msg,
            GameMsg = State#state.game_msg,
            Pid ! {Msg, GameMsg},
            receiver(#state{});
        stop ->
            ok;
        _ ->
            receiver(State)
    end.

get_all_countries() ->
    [england, germany, france, italy, russia, turkey, austria].