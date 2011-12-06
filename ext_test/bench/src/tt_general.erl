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
%%% @doc Driver for the general flow
%%%
%%% This module is to be run by basho bench
%%%
%%% @since : 22 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(tt_general).

-export([new/1, run/4]).

-include("basho_bench.hrl").

-define(GAME_COUNT, 1).

-record(state, {users, orders, node, last_phase,
                waiting_games = [], ongoing_games = []}).
-record(load_user, {nick, session, pid}).
-record(waiting_game, {id, to_join, joined = []}).
-record(ongoing_game, {id, players = [],
                       phase = {order_phase, spring, 1901},
                       to_send,
                       sent = []}).

setup(1) ->
    setup_fun();
setup(Tries) ->
    try setup_fun()
    catch
        _:_ ->
            setup(Tries-1)
    end.

setup_fun() ->
    Node = basho_bench_config:get(tt_node),
    load_test:connect(Node, 30),

    Users = load_test:register_user(7),
    UserData = load_test:login(Users),

    Nicks = load_test:get_user_nicks(Users),
    Sessions = load_test:get_user_sessions(UserData),
    Pids = load_test:get_user_pids(UserData),
    LoadUsers = lists:map(fun({Nick, Session, Pid}) ->
                              #load_user{nick = Nick,
                                         session = Session,
                                         pid = Pid}
                      end, lists:zip3(Nicks, Sessions, Pids)),
    Orders = load_test:create_orders(),
    LastDict = lists:last(Orders),
    {ok, Phase} = dict:find(phase, LastDict),
    {ok, Season} = dict:find(season, LastDict),
    {ok, Year} = dict:find(year, LastDict),

    LastPhase = {Phase, Season, Year},
    {ok, #state{users = LoadUsers, orders=Orders, node=Node,
                last_phase = LastPhase}}.

%%-------------------------------------------------------------------
%% @doc
%% Initialization
%% * Setup 7 users
%% * Get game orders
%% @end
%%-------------------------------------------------------------------
new(_Id) ->
    setup(10).

run(create_game, _KeyGen, _ValueGen, State) ->
    Session = random_session(State),
    GameId = load_test:create_game(Session),

    Users = State#state.users,
    Countries = load_test:get_all_countries(),
    ToJoin = lists:zip(Users, Countries),

    Game = #waiting_game{id = GameId, to_join = ToJoin},
    {ok, add_waiting(State, Game)};

run(join_game, _KeyGen, _ValueGen,
    State = #state{waiting_games = Waiting,
                   ongoing_games = Ongoing,
                   node = Node}) ->
    case Waiting of
        [] ->
            {ok, State};
        [Game | Games] ->
            GameId = Game#waiting_game.id,
            [{User, Country} | Rest] = Game#waiting_game.to_join,
            load_test:join_game(user_session(User),
                                GameId,
                                Country),
            OldJoined = Game#waiting_game.joined,
            Joined = [{User, Country} | OldJoined],
            case Rest of
                [] ->
                    NewGame = #ongoing_game{id = GameId,
                                            players = Joined,
                                            to_send = Joined},
                    load_test:start_game(Node, GameId),
                    {ok, State#state{waiting_games = Games,
                                     ongoing_games = Ongoing ++ [NewGame]}};
                Rest ->
                    Game1 = Game#waiting_game{to_join = Rest,
                                              joined = Joined},
                    {ok, State#state{waiting_games = [Game1|Games]}}
            end
    end;

run(in_game_msg, _KeyGen, _ValueGen, State) ->
    case random_game(State) of
        empty ->
            {ok, State};
        Game ->
            Players = game_players(Game),
            {FromUser, _} = random_elem(Players),
            {ToUser, ToCountry} = random_elem(Players),

            load_test:send_game_msg(user_session(FromUser),
                                    game_id(Game),
                                    [ToCountry]),
            case load_test:game_message_receiver(user_pid(ToUser)) of
                {ok, success} ->
                    {ok, State};
                _ ->
                    {error, in_game_msg_not_received, State}
            end
    end;

run(send_order, _KeyGen, _ValueGen,
    State = #state{ongoing_games = Ongoing,
                   orders = Orders,
                   node = Node,
                   last_phase = LastPhase}) ->
    case random_game(State) of
        empty ->
            {ok, State};
        Game ->
            GameId = game_id(Game),
            Phase = game_phase(Game),
            [{User, Country} | Rest] = game_to_send(Game),
            load_test:send_order(GameId, Orders, Phase,
                                 {user_session(User), Country}),
            Game1 = case Rest of
                        [] ->
                            load_test:phase_change(Node, GameId),
                            case next_phase(Phase) of
                                LastPhase -> 
                                    remove_game;
                                Phase1 ->
                                    Players = game_players(Game),
                                    Game#ongoing_game{to_send = Players,
                                                      phase = Phase1,
                                                      sent = []}
                            end;
                        Rest ->
                            OldSent = game_sent(Game),
                            Sent = [{User, Country} | OldSent],
                            Game#ongoing_game{to_send = Rest,
                                              sent = Sent}
                    end,
            Ongoing1 = lists:delete(Game, Ongoing),
            case Game1 of
                remove_game ->
                    {ok, State#state{ongoing_games = Ongoing1}};
                _ ->
                    {ok, State#state{ongoing_games = [Game1|Ongoing1]}}
            end
    end;

run(search, _KeyGen, _ValueGen, State) ->
    Session = random_session(State),
    Games = load_test:search_current(Session),
    case is_list(Games) of
        true ->
            {ok, State};
        false ->
            {error, search_failed, State}
    end;

run(off_game_msg, _KeyGen, _ValueGen, State) ->
    Session = random_session(State),
    User = random_user(State),
    Nick = user_nick(User),
    Pid = user_pid(User),

    load_test:send_msg(Session, Nick),
    case load_test:user_message_receiver(Pid) of
        {ok, _} ->
            {ok, State};
        _ ->
            {error, off_game_msg_not_received, State}
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% random value between 1..length(List), and 0 if list is empty.
%% @end
%%-------------------------------------------------------------------
random_val([]) ->
    0;
random_val(List) ->
    random:uniform(length(List)).

%%-------------------------------------------------------------------
%% @doc
%% random element from the list, and 'empty' if list is empty.
%% @end
%%-------------------------------------------------------------------
random_elem([]) ->
    empty;
random_elem(List) ->
    lists:nth(random_val(List), List).

%%-------------------------------------------------------------------
%% @doc
%% random session from the users in the state record
%% @end
%%-------------------------------------------------------------------
random_session(#state{users = Users}) ->
    user_session(random_elem(Users)).

%%-------------------------------------------------------------------
%% @doc
%% random user from the users in the state record
%% @end
%%-------------------------------------------------------------------
random_user(#state{users = Users}) ->
    random_elem(Users).

%%-------------------------------------------------------------------
%% @doc
%% random ongoing game from the state record
%% @end
%%-------------------------------------------------------------------
random_game(#state{ongoing_games = Games}) ->
    random_elem(Games).

%%-------------------------------------------------------------------
%% @doc
%% add a game to the waiting_games list in the state record
%% @end
%%-------------------------------------------------------------------
add_waiting(State = #state{waiting_games = Games}, Game) ->
    State#state{waiting_games = Games ++ [Game]}.


%%-------------------------------------------------------------------
%% @doc
%% Little helper function, that returns the next phase.
%% @end
%%-------------------------------------------------------------------
next_phase({order_phase, Season, Year}) ->
    {retreat_phase, Season, Year};
next_phase({retreat_phase, spring, Year}) ->
    {order_phase, fall, Year};
next_phase({retreat_phase, fall, Year}) ->
    {build_phase, fall, Year};
next_phase({build_phase, fall, Year}) ->
    {order_phase, spring, Year+1}.

%%-------------------------------------------------------------------
%% Record functions
%%-------------------------------------------------------------------
user_nick(#load_user{nick = Nick}) ->
    Nick.
user_session(#load_user{session = Session}) ->
    Session.
user_pid(#load_user{pid = Pid}) ->
    Pid.

game_id(#ongoing_game{id = Id}) ->
    Id.

game_players(#ongoing_game{players = Players}) ->
    Players.

game_phase(#ongoing_game{phase = Phase}) ->
    Phase.

game_to_send(#ongoing_game{to_send = ToSend}) ->
    ToSend.

game_sent(#ongoing_game{sent = Sent}) ->
    Sent.
