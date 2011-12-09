-module(backend_test).

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
-define(JSON_CREATE_GAME(SessId),
        {struct,
         [{action,<<"create_game">>},
          {data,
           [
            {struct,[{session_id, SessId}]},
            {struct,[{name, <<"fault_tolerance_test">>}]},
            {struct,[{description,
                      list_to_binary(
                        io_lib:format("A game for testing ressurection. "
                                      "Created ~p",
                                      [calendar:local_time()]))
                     }]},
            {struct,[{password, <<"">>}]},
            {struct,[{press, <<"white">>}]},
            {struct,[{order_phase, <<"1">>}]},
            {struct,[{retreat_phase, <<"1">>}]},
            {struct,[{build_phase, <<"1">>}]},
            {struct,[{waiting_time, <<"1">>}]},
            {struct,[{num_players, <<"2">>}]}
           ]
          }
         ]
        }).
-define(JSON_JOIN(SessId, GameId),
        {struct,
         [{action, <<"join_game">>},
          {data,
           [{struct, [{session_id, SessId}]},
            {struct, [{game_id, GameId}]},
            {struct, [{country, <<"england">>}]}
           ]
          }
         ]
        }).
-define(JSON_GAME_OVERVIEW(SessId, GameId),
        {struct,
         [{action, <<"game_overview">>},
          {data,
           [{struct, [{session_id, SessId}]},
            {struct, [{game_id, GameId}]}
           ]
          }
         ]
        }).

backend_test_() ->
    only_from_escript({setup,
                       fun backend_test_setup/0,
                       fun backend_test_teardown/1,
                       fun backend_test_instantiator/1}).

backend_test_setup() ->
    ok = application:start(fault_tolerance),
    os:cmd("epmd -daemon"), % net_kernel needs epmd.
    net_kernel:start([fault_tolerance_test, longnames]),
    erlang:set_cookie(node(), 'treacherous_talks'),
    %% This env var would normally be set by the fault_tolerance escript.
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

    %% --- REGISTER (but don't care if nick taken) ---
    WebAddr = get_web_addr(ClustConf),
    {ok, _Pid} = test_client:connect(WebAddr, 8000),
    web_frontend_send(?JSON_REGISTER),
    _RegResult = web_frontend_recv(),
    %%?debugVal(RegResult),

    %% --- LOGIN ---
    SessId = login(),

    %% --- CREATE GAME ---
    web_frontend_send(?JSON_CREATE_GAME(SessId)),
    CreateReplyTerm = web_frontend_recv(),
    GameId = event_create_get_gameid(CreateReplyTerm),
    ?debugVal(GameId),

    %% --- JOIN GAME ---
    web_frontend_send(?JSON_JOIN(SessId, GameId)),
    _JoinGameReply = web_frontend_recv(),
    %%?debugVal(JoinGameReply),

    {WebAddr, SessId, GameId, ClustConf}.

backend_test_instantiator({WebAddr, SessId, GameId, ClustConf}) ->
    {timeout, 60 * 10,
     [
      fun() ->
              StartupOrder = cluster_utils:generate_startup_order(ClustConf),
              wait_seconds(65), % wait for game to start

              %% --- INITIAL GAME STATE ---
              prettyp_game_info(get_game_info(SessId, GameId)),

              %% --- BACKEND NODE STUFF ---
              [BackendNodeA, BackendNodeB] =
                  backend_nodes_from_startup(StartupOrder),

              %% --- HALT BACKEND A ---
              halt_node(BackendNodeA),
              wait_seconds(65), % give BackendB time to notice A is gone.
                                % give pg2 time to realise the backend is gone
              test_client:close(),

              %% --- LOGIN 2 ---
              {ok, _Pid} = test_client:connect(WebAddr, 8000),
              SessId2 = login(),

              prettyp_game_info(get_game_info(SessId2, GameId)),

              %% --- START BACKEND A ---
              %%PingResult = cluster_utils:do_action_on_releases(StartupOrder, ping_release),
              %%?debugVal(PingResult),
              StartResult = cluster_utils:do_action_on_releases(StartupOrder, start_release),
              ?debugVal(StartResult),
              NotifyResult = cluster_utils:notify_backends(ClustConf),
              ?debugVal(NotifyResult),
              %%PingResult2 = cluster_utils:do_action_on_releases(StartupOrder, ping_release),
              %%?debugVal(PingResult2),

              wait_seconds(65), % give BackendB time to sync with BackendA

              prettyp_game_info(get_game_info(SessId2, GameId)),

              %% --- HALT BACKEND B ---
              halt_node(BackendNodeB),
              wait_seconds(65), % give BackendA time to notice B is gone.
                                % give pg2 time to realise the backend is gone
              test_client:close(),

              %% --- LOGIN 3 ---
              {ok, _Pid2} = test_client:connect(WebAddr, 8000),
              SessId3 = login(),

              %% --- RECOVERY CHECK START ---
              ?debugMsg("RECOVERY CHECK START - GameInfoStart:"),
              GameInfoStart = get_game_info(SessId3, GameId),
              prettyp_game_info(GameInfoStart),

              %% --- WAIT FOR PHASE CHANGE ---
              wait_seconds(120),

              %% --- RECOVERY CHECK END ---
              ?debugMsg("RECOVERY CHECK END - GameInfoEnd"),
              GameInfoEnd = get_game_info(SessId3, GameId),
              prettyp_game_info(GameInfoEnd),

              ?assertNot(GameInfoStart == GameInfoEnd)
      end
     ]}.

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

%% Return the given test if it should be run, or a noop fun
%% if the test shouldn't be run because it's not being
%% run from the expected escript.
%%
%% This allows the eunit tests to be released without being run
%% in our top-level test run.
only_from_escript(Test) ->
    ScriptName = lists:flatten(escript:script_name()),
    ?debugVal(ScriptName),
    Match = re:run(ScriptName, "fault_tolerance"),
    case Match of
        {match, _} ->
            Test;
        _ ->
            ?debugFmt("The following test in module ~p is being skipped.~n"
                      "This eunit run is from ~p but this test is only intended "
                      "to run only from the \"fault_tolerance\" escript.~n",
                      [?MODULE, ScriptName]),
            %% TODO: give better output of the test to identify which test it is
            %% with minimal effort.
            ?debugVal(Test),
            fun() -> ok end % return noop fun for noop test
    end.

get_web_addr(ClustConf) ->
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    [WebHost] = [Host || {Host, _SysManPfx, Rel, _RelPfx} <- StartupOrder, Rel == web_frontend],
    {host, WebHost, _SysManPfx, WebHostConf} = lists:keyfind(WebHost, 2, ClustConf),
    {release, web_frontend, _RelPfx, WebFERelConf} = lists:keyfind(web_frontend, 2, WebHostConf),
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

login() ->
    web_frontend_send(?JSON_LOGIN),
    LoginReplyTerm = web_frontend_recv(),
    SessId = event_login_get_session(LoginReplyTerm),
    ?debugVal(SessId),
    SessId.

get_game_info(SessId, GameId) ->
    web_frontend_send(?JSON_GAME_OVERVIEW(SessId, GameId)),
    GameOverviewReply = web_frontend_recv(),
    GameInfo = event_gameoverview_get_gameinfo(GameOverviewReply),
    GameInfo.

prettyp_game_info(GameInfo) ->
    ?debugFmt("~n~n= GAME INFO: =====~n~s", [GameInfo]).

wait_seconds(Seconds) ->
    ?debugFmt("Waiting ~p seconds...~n", [Seconds]),
    timer:sleep(1000 * Seconds).

halt_node(Node) ->
    ?debugFmt("Halting ~p~n", [Node]),
    %% it should be up before we halt it for correctness
    pong = net_adm:ping(Node),
    %% it should be down after we halt it
    {badrpc,nodedown} = rpc:call(Node, erlang, halt, []).

%% Assumes exactly two backends for now.
backend_nodes_from_startup(StartupOrder) ->
    BackendStartups = [X || X={_, _, backend, _} <- StartupOrder],
    [BackendA, BackendB] = BackendStartups,
    {HostA, _SysManPfxA, backend, RelPfxA} = BackendA,
    {HostB, _SysManPfxB, backend, RelPfxB} = BackendB,
    BackendNodeA = list_to_atom(atom_to_list(RelPfxA) ++ "@" ++ HostA),
    BackendNodeB = list_to_atom(atom_to_list(RelPfxB) ++ "@" ++ HostB),
    [BackendNodeA, BackendNodeB].

%%------------------------------------------------------------------------------
%% JSON term helpers.
%%
%% Each takes a mochijson2 decoded json term
%% and returns the interesting value.
%%------------------------------------------------------------------------------

event_login_get_session(JSONTerm) ->
    {struct,[{<<"event">>,<<"login_success">>},
             {<<"event_data">>, {struct,EventData}},
             _, _, _
            ]} = JSONTerm,
    {<<"session_id">>, SessionIdBin} =
        lists:keyfind(<<"session_id">>, 1, EventData),
    SessionIdBin.

event_create_get_gameid(JSONTerm) ->
    {struct,[{<<"event">>,<<"create_game_success">>},
             _,_,_,
             {<<"message_value">>,SuccessMsg}
            ]} = JSONTerm,
    {match, [GameId]} =
        re:run(SuccessMsg, "\"(.*)\"", [{capture, all_but_first, binary}]),
    GameId.

event_gameoverview_get_gameinfo(JSONTerm) ->
    {struct,[{<<"event">>,<<"game_overview_success">>},
             {<<"event_data">>, {struct, EventData}},
             _,_,_
            ]} = JSONTerm,
    {<<"game_info">>, GameInfo} =
        lists:keyfind(<<"game_info">>, 1, EventData),
    GameInfo.
