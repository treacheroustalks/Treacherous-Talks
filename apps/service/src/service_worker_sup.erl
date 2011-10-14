%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Provides functionality that is used by the worker supervisors
%%% of a service applications.
%%% @end
%%%
%%% @since : 14 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(service_worker_sup).

%% Public interface
-export([worker_count/1, worker_count/4, create_childspec/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, I, Type), {Id, {I, start_link, []}, permanent, 5000, Type, [I]}).


%%-------------------------------------------------------------------
%% @doc
%% Returns the number of workers for a given supervisor.
%% @end
%% @spec worker_count(Sup::atom()) -> integer()
%% @end
%%-------------------------------------------------------------------
-spec worker_count(atom()) -> integer().
worker_count(Sup) ->
    [_, _, _, {workers, Count}] = supervisor:count_children(Sup),
    Count.

%%-------------------------------------------------------------------
%% @doc
%% Changes the number of workers for a given supervisor. App specifies
%% the application name they run for and Worker the worker module.
%% @end
%% @spec worker_count(Sup::atom(), App::atom(), Worker::atom(), Count::integer()) ->
%%       integer()
%% @end
%%-------------------------------------------------------------------
-spec worker_count(atom(), atom(), atom(), integer()) -> integer().
worker_count(Sup, App, Worker, Count) when Count >= 0 ->
    OldCount = worker_count(Sup),
    application:set_env(App, Worker, Count),
    Diff = Count - OldCount,
    if
        Diff == 0 ->
            ok;
        Diff >= 0 ->
            lists:foreach(fun (Id) ->
                                  supervisor:start_child(
                                    Sup, ?CHILD(Id, Worker, worker))
                          end, lists:seq(OldCount + 1, Count));
        true ->
            lists:foreach(fun (Id) ->
                                  supervisor:terminate_child(Sup, Id),
                                  supervisor:delete_child(Sup, Id)
                          end, lists:seq(Count + 1, OldCount) )
    end,
    worker_count(Sup).

%%-------------------------------------------------------------------
%% @doc
%% Creates a list child_specs for the application App. Worker is the
%% worker module and WorkerConf the config key.
%% @end
%% @spec create_childspec(App::atom(), WorkerConf::atom(), Worker::atom()) ->
%%       [child_spec()]
%% @end
%%-------------------------------------------------------------------
create_childspec(App, WorkerConf, Worker) ->
    {ok, WorkersCount} = application:get_env(App, WorkerConf),
    [ ?CHILD(Id, Worker, worker) || Id <- lists:seq(1, WorkersCount) ].
