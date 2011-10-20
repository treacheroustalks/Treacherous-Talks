-module(game_timer_sup).
-behaviour(supervisor).

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
    io:format ("[~p] starting ~p~n", [?MODULE, self()]),
    TimerSpec = {game_timer, {game_timer, start_link, []},
                 permanent, 5000, worker, [game_timer]},
    {ok, { {simple_one_for_one, 5, 10}, [TimerSpec] } }.
