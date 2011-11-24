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

-record(state, {sessions, orders, node, pids, creator}).

%%-------------------------------------------------------------------
%% @doc
%% Initialization
%% * Setup 7 users
%% * Get game orders
%% @end
%%-------------------------------------------------------------------
new(_Id) ->
    Node = basho_bench_config:get(tt_node),
    pg2:start(),
    pong = net_adm:ping(Node),

    Users = load_test:register_user(7),
    UserData = load_test:login(Users),
    Sessions = load_test:get_user_sessions(UserData),
    Pids = load_test:get_user_pids(UserData),
    Orders = load_test:create_orders(),
    {Nick, _Password} = hd(Users),
    {SessionId, Pid} = hd(UserData),
    {ok, #state{sessions=Sessions, orders=Orders, node=Node, pids=Pids,
                creator={Nick, SessionId, Pid}}}.

%%-------------------------------------------------------------------
%% @doc
%% Run
%% * Setup a new game
%% * Send orders for 1 year, including sending game messages
%% * Search for the created game
%% * Send user message (to self)
%% @end
%%-------------------------------------------------------------------
run(test, _KeyGen, _ValueGen, State) ->
    % Game setup
    Sessions = State#state.sessions,
    Orders = State#state.orders,
    Node = State#state.node,
    {Nick, SessionId, Pid} = State#state.creator,
    Pids = State#state.pids,
    GameId = load_test:create_game(SessionId),
    GameUsers = load_test:join_full_game(Sessions, GameId),
    load_test:start_game(Node, GameId),

    % Run game for 1 year
    Phases = load_test:get_phase_years(1),
    PhaseRet = run_phases(Node, Pids, GameId, Orders, Phases, GameUsers),

    % Search
    Games = load_test:search_current(SessionId),
    GamesRet = length(Games),

    % Send message
    load_test:send_msg(SessionId, Nick),
    MsgRet = load_test:user_message_receiver(Pid),

    case {PhaseRet, GamesRet, MsgRet} of
        {{ok, _}, 0, {ok, _}} ->
            {error, game_search_fail};
        {{ok, _}, _, {ok, _}} ->
            {ok, State};
        {{error, Error}, _, _} ->
            {error, Error};
        {_, _, {error, Error}} ->
            {error, Error};
        _ ->
            {error, unknown_failure}
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
run_phases(_Node, _Pids, _GameId, _Orders, [], _GameUsers) ->
    {ok, success};
run_phases(Node, Pids, GameId, Orders, [Phase|Phases], GameUsers) ->
    case run_phase(Node, Pids, GameId, Orders, Phase, GameUsers) of
        {ok, success} ->
            run_phases(Node, Pids, GameId, Orders, Phases, GameUsers);
        Error ->
            Error
    end.

% Each phase consists of
% * All countries send orders
% * All countries send messages to each other
% * Game phase moved to next phase
run_phase(Node, Pids, GameId, Orders, Phase, GameUsers) ->
    load_test:send_orders(GameId, Orders, Phase, GameUsers),

    Sessions = load_test:get_user_sessions(GameUsers),
    case send_messages(Sessions, GameId, Pids) of
        {ok, success} ->
            load_test:phase_change(Node, GameId),
            {ok, success};
        Error ->
            Error
    end.

send_messages([], _GameId, _Pids) ->
    {ok, success};
send_messages([SessionId|Sessions], GameId, Pids) ->
    case send_message(SessionId, GameId, Pids) of
        {ok, _} ->
            send_messages(Sessions, GameId, Pids);
        Error ->
            Error
    end.

send_message(SessionId, GameId, Pids) ->
    load_test:send_game_msg(SessionId, GameId, load_test:get_all_countries()),
    case load_test:game_multiple_message_receiver(Pids) of
        {ok, success} ->
            {ok, success};
        Error ->
            Error
    end.