-module(service_conf).

-export([node_pids/1, node_count/1,
         worker_count/1, worker_count/2]).

node_pids(Name) ->
    pg2:get_members(Name).

node_count(Name) ->
    length(node_pids(Name)).

worker_count(Pid) ->
    gen_server:call(Pid, worker_count).

worker_count(Pid, Count) ->
    gen_server:call(Pid, {worker_count, Count}).

