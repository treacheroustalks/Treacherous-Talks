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
%%% @doc Driver for game play flow
%%%
%%% This module is to be run by basho bench
%%%
%%% @since : 22 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(tt_game_play).

-export([new/1, run/4]).

-include("basho_bench.hrl").

-record(state, {sessions, orders, node}).

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
    Orders = load_test:create_orders(),
    {ok, #state{sessions=Sessions, orders=Orders, node=Node}}.

%%-------------------------------------------------------------------
%% @doc
%% Run
%% * Setup new game
%% * Send orders between users for 1 year period
%% @end
%%-------------------------------------------------------------------
run(test, _KeyGen, _ValueGen, State) ->
    Sessions = State#state.sessions,
    Orders = State#state.orders,
    Node = State#state.node,
    GameId = load_test:create_game(hd(Sessions)),
    GameUsers = load_test:join_full_game(Sessions, GameId),
    load_test:start_game(Node, GameId),

    % Run game for 1 year
    Phases = load_test:get_phase_years(1),
    lists:foreach(
      fun(Phase) ->
              load_test:send_orders(GameId, Orders, Phase, GameUsers),
              load_test:phase_change(Node, GameId)
      end, Phases),
    {ok, State}.