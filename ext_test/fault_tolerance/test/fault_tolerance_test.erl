-module(fault_tolerance_test).

-include_lib("eunit/include/eunit.hrl").

-define(JSON_LOGIN,
        {struct,
         [{action,<<"login">>},
          {data,
           [{struct,[{nick,<<"fault_test">>}]},
            {struct,[{password,<<"password">>}]}
           ]
          }
         ]
        }).
-define(JSON_REGISTER,
        {struct,
         [{action,<<"register">>},
          {data,
           [{struct,[{nick,<<"fault_test">>}]},
            {struct,[{password,<<"password">>}]},
            {struct,[{email,<<"fault_test@mail.pcs">>}]},
            {struct,[{name,<<"Fault Test User">>}]}
           ]
          }
         ]
        }).

backend_test_() ->
    {setup,
     fun backend_test_setup/0,
     fun backend_test_teardown/1,
     {timeout, 60, fun backend_test_tst/0}}.

backend_test_setup() ->
    ok = application:start(fault_tolerance),
    net_kernel:start([fault_tolerance_test, longnames]),
    erlang:set_cookie(node(), 'treacherous_talks'),

    {ok, ClustConf} = application:get_env(fault_tolerance, test_cluster_config),
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    ProcessedConfig = cluster_utils:preprocess_clustconf(ClustConf),
    DistribConfResult = cluster_utils:distribute_config(ProcessedConfig),

    ?debugVal(DistribConfResult),
    StartResult = cluster_utils:do_action_on_releases(StartupOrder, start_release),
    ?debugVal(StartResult),
    NotifyResult = cluster_utils:notify_backends(ClustConf),
    ?debugVal(NotifyResult),
    PingResult = cluster_utils:do_action_on_releases(StartupOrder, ping_release),
    ?debugVal(PingResult),

    WebAddr = get_web_addr(ClustConf),
    {ok, _Pid} = test_client:connect(WebAddr, 8000),
    web_frontend_send(?JSON_REGISTER),
    RegResult = web_frontend_recv(),
    ?debugVal(RegResult).

backend_test_tst() ->
    {ok, ClustConf} = application:get_env(fault_tolerance, test_cluster_config),

    web_frontend_send(?JSON_LOGIN),

    ReplyTerm = web_frontend_recv(),
    ?debugVal(ReplyTerm).

backend_test_teardown(_) ->
    {ok, ClustConf} = application:get_env(fault_tolerance, test_cluster_config),
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    ShutdownOrder = lists:reverse(StartupOrder),
    StopResult = cluster_utils:do_action_on_releases(ShutdownOrder, stop_release),
    ?debugVal(StopResult),
    application:stop(fault_tolerance),
    net_kernel:stop().


%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

get_web_addr(ClustConf) ->
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    ?debugVal(StartupOrder),
    [WebHost] = [Host || {Host, _SysManPfx, Rel} <- StartupOrder, Rel == web_frontend],
    {host, WebHost, _SysManPfx, WebHostConf} = lists:keyfind(WebHost, 2, ClustConf),
    {release, web_frontend, WebFERelConf} = lists:keyfind(web_frontend, 2, WebHostConf),
    {web_frontend, WebFEConf} = lists:keyfind(web_frontend, 1, WebFERelConf),
    {listen, [WebAddr]} = lists:keyfind(listen, 1, WebFEConf),
    WebAddr.

web_frontend_send(JSONTerm) ->
    JSONString = mochijson2:encode(JSONTerm),
    JSONBin = list_to_binary(JSONString),
    test_client:send({text, JSONBin}).

web_frontend_recv() ->
    {text, Reply} = test_client:recv(),
    ReplyJSON = binary_to_list(Reply),
    ReplyTerm = mochijson2:decode(ReplyJSON),
    ReplyTerm.
