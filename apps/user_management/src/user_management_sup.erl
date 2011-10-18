%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Top supervisor of the user management.
%%%
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(user_management_sup).
-behaviour(supervisor).

%% Public interface
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Public interface
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(no_arg) ->
    {ok, { {one_for_one, 5, 10},
           [
            ?CHILD(user_management_worker_sup, supervisor),
            ?CHILD(user_management_config, worker)
           ]} }.

