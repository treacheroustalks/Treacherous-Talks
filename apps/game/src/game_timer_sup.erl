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
-module(game_timer_sup).
-behaviour(supervisor).

-include_lib("utils/include/debug.hrl").

-include_lib ("datatypes/include/game.hrl").

%% API
-export([start_link/0, worker_count/0, create_timer/1,
         terminate_timer/1, restart_timer/1, delete_timer/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

worker_count() ->
    service_worker_sup:worker_count(?MODULE).

%%-------------------------------------------------------------------
%% @doc
%% Starts a new timer, for the game Game
%% @end
%% [@spec create_timer(Game::#game{}) -> {ok, Timer::pid()}.
%% @end]
%%-------------------------------------------------------------------
-spec create_timer(#game{}) -> {ok, pid()}.
create_timer(Game) ->
    supervisor:start_child(?MODULE, [Game]).

terminate_timer(Game) ->
    supervisor:terminate_child(?MODULE, Game#game.id).

restart_timer(Game) ->
    supervisor:restart_child(?MODULE, Game#game.id).

delete_timer(Game) ->
    supervisor:delete_child(?MODULE, Game#game.id).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(no_arg) ->
    ?DEBUG("[~p] starting ~p~n", [?MODULE, self()]),
    TimerSpec = {game_timer, {game_timer, start_link, []},
                 transient, 5000, worker, [game_timer]},
    {ok, { {simple_one_for_one, 5, 10}, [TimerSpec] } }.
