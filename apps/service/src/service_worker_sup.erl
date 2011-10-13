-module(service_worker_sup).

%% API
-export([worker_count/1, worker_count/4, create_childspec/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, I, Type), {Id, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
worker_count(Sup) ->
    [_, _, _, {workers, Count}] = supervisor:count_children(Sup),
    Count.

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

create_childspec(App, WorkerConf, Worker) ->
    {ok, WorkersCount} = application:get_env(App, WorkerConf),
    [ ?CHILD(Id, Worker, worker) || Id <- lists:seq(1, WorkersCount) ].
