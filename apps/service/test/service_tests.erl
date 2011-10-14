-module(service_tests).
-include_lib("eunit/include/eunit.hrl").

service_worker_test() ->
    Group = test_worker,

    service_worker:join_group(Group),
    ?assertEqual([self()], service_worker:get_members(Group)),
    
    service_worker:join_group(Group),
    ?assertEqual([self(), self()], service_worker:get_members(Group)),

    ?assertEqual(self(), service_worker:select_pid(Group)).
    

service_conf_test() ->
    Group = test_conf,

    service_worker:join_group(Group),
    ?assertEqual([self()], service_conf:node_pids(Group)),

    service_worker:join_group(Group),
    ?assertEqual([self(), self()], service_conf:node_pids(Group)),

    ?assertEqual(2, service_conf:node_count(Group)).
