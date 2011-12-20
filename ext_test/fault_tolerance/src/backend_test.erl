-module(backend_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/game.hrl").

-export([all_tests/0]).

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
                        io_lib:format("A game for testing resurrection. "
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
-define(JSON_JOIN(SessId, GameIdBin),
        {struct,
         [{action, <<"join_game">>},
          {data,
           [{struct, [{session_id, SessId}]},
            {struct, [{game_id, GameIdBin}]},
            {struct, [{country, <<"england">>}]}
           ]
          }
         ]
        }).
-define(JSON_GAME_OVERVIEW(SessId, GameIdBin),
        {struct,
         [{action, <<"game_overview">>},
          {data,
           [{struct, [{session_id, SessId}]},
            {struct, [{game_id, GameIdBin}]}
           ]
          }
         ]
        }).


all_tests() ->
    {setup,
     fun env_setup/0,
     fun env_teardown/1,
     [
      backend_wait(),
      backend_quick([{spring, order_phase},
                     {spring, retreat_phase},
                     {fall, order_phase},
                     {fall, retreat_phase},
                     {fall, build_phase}])
     ]
    }.

backend_wait() ->
    {setup,
     fun backend_test_setup/0,
     fun backend_test_teardown/1,
     fun backend_wait_instantiator/1}.

backend_quick(GameStates) ->
    {foreach,
     fun backend_test_setup/0,
     fun backend_test_teardown/1,
     make_specific_phase_instantiators(GameStates)}.

env_setup() ->
    %% --- APPS and NETWORKING ---
    ok = application:start(fault_tolerance),
    os:cmd("epmd -daemon"), % net_kernel needs epmd.
    net_kernel:start([fault_tolerance_test, longnames]),
    erlang:set_cookie(node(), 'treacherous_talks').

env_teardown(_) ->
    application:stop(fault_tolerance),
    net_kernel:stop().

backend_test_setup() ->
    %% --- SETUP CLUSTER ---
    %% This env var would normally be set by the fault_tolerance escript.
    {ok, ClustConf} = application:get_env(fault_tolerance, test_cluster_config),
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    ProcessedConfig = cluster_utils:preprocess_clustconf(ClustConf),
    WebAddr = get_web_addr(ClustConf),

    DistribConfResult = cluster_utils:distribute_config(ProcessedConfig),
    ?debugFmt("DistribConfResult =~n~p", [DistribConfResult]),
    StartResult = cluster_utils:do_action_on_releases(StartupOrder, start_release),
    ?debugFmt("StartResult =~n~p", [StartResult]),
    NotifyResult = cluster_utils:notify_backends(ClustConf),
    ?debugFmt("NotifyResult =~n~p", [NotifyResult]),
    PingResult = cluster_utils:do_action_on_releases(StartupOrder, ping_release),
    ?debugFmt("PingResult =~n~p", [PingResult]),

    %% --- REGISTER (but don't care if nick taken) ---
    {ok, _Pid} = test_client:connect(WebAddr, 8000),
    web_frontend_send(?JSON_REGISTER),
    _RegResult = web_frontend_recv(),

    %% --- LOGIN ---
    SessId = login(),

    %% --- CREATE GAME ---
    web_frontend_send(?JSON_CREATE_GAME(SessId)),
    CreateReplyTerm = web_frontend_recv(),
    GameIdBin = event_create_get_gameid(CreateReplyTerm),
    ?debugVal(GameIdBin),

    %% --- JOIN GAME ---
    web_frontend_send(?JSON_JOIN(SessId, GameIdBin)),
    _JoinGameReply = web_frontend_recv(),

    {WebAddr, SessId, GameIdBin, ClustConf, StartupOrder}.

backend_test_teardown(_) ->
    test_client:close(),
    {ok, ClustConf} = application:get_env(fault_tolerance, test_cluster_config),
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    ShutdownOrder = lists:reverse(StartupOrder),
    StopResult = cluster_utils:do_action_on_releases(ShutdownOrder, stop_release),
    ?debugVal(StopResult).

backend_wait_instantiator({WebAddr, SessId, GameIdBin, ClustConf, StartupOrder}) ->
    {timeout, 60 * 10,
     [
      {"Test, as realistically as possible, that the backend hosting the game "
       "fails unexpectendly and that the game continues on the other backend.",
       fun() ->
               wait_seconds(65), % wait for game to start

               %% --- INITIAL GAME STATE ---
               prettyp_game_state(get_game_state(SessId, GameIdBin)),

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

               prettyp_game_state(get_game_state(SessId2, GameIdBin)),

               %% --- START BACKEND A ---
               StartResult = cluster_utils:do_action_on_releases(StartupOrder,
                                                                 start_release),
               ?debugFmt("StartResult =~n~p", [StartResult]),
               NotifyResult = cluster_utils:notify_backends(ClustConf),
               ?debugFmt("NotifyResult =~n~p", [NotifyResult]),

               wait_seconds(65), % give BackendB time to sync with BackendA

               prettyp_game_state(get_game_state(SessId2, GameIdBin)),

               %% --- HALT BACKEND B ---
               halt_node(BackendNodeB),
               wait_seconds(65), % give BackendA time to notice B is gone.
                                 % give pg2 time to realise the backend is gone
               test_client:close(),

               %% --- LOGIN 3 ---
               {ok, _Pid2} = test_client:connect(WebAddr, 8000),
               SessId3 = login(),

               %% --- RECOVERY CHECK START ---
               ?debugMsg("RECOVERY CHECK START - GameStateStart:"),
               GameStateStart = get_game_state(SessId3, GameIdBin),
               prettyp_game_state(GameStateStart),

               %% --- WAIT FOR PHASE CHANGE ---
               wait_seconds(90),

               %% --- RECOVERY CHECK END ---
               ?debugMsg("RECOVERY CHECK END - GameStateEnd"),
               GameStateEnd = get_game_state(SessId3, GameIdBin),
               prettyp_game_state(GameStateEnd),

               %% just for tester
               ?debugVal((GameStateStart == GameStateEnd)),
               ?assertNot(GameStateStart == GameStateEnd)
       end}
     ]}.

%%------------------------------------------------------------------------------
%% For each {Season, Phase} in GameStates, create a test
%% that will work out on which backend the game is, halt that backend,
%% and check whether the game then continues changing phases by itself.
%%------------------------------------------------------------------------------
make_specific_phase_instantiators(GameStates) ->
    InstantiatorMaker =
        fun({Season, Phase}) ->
                fun({WebAddr, SessId, GameIdBin, _ClustConf, StartupOrder}) ->
                        {lists:flatten(io_lib:format("Resurrection when game is in ~p",
                                                     [{Season, Phase}])),
                         {timeout, 60 * 10,
                          fun() ->
                                  GameId = list_to_integer(binary_to_list(GameIdBin)),
                                  [BackendNodeA, _BackendNodeB] = Backends =
                                      backend_nodes_from_startup(StartupOrder),

                                  advance_to_state(BackendNodeA, GameId, spring, order_phase),
                                  wait_seconds(5), % wait for timer to init
                                  prettyp_game_state(get_game_state(SessId, GameIdBin)),

                                  GameBackend = find_game_backend(Backends, GameId),

                                  ?debugFmt("Game ~p is on ~p~n", [GameIdBin, GameBackend]),

                                  advance_to_state(BackendNodeA, GameId, Season, Phase),
                                  prettyp_game_state(get_game_state(SessId, GameIdBin)),

                                  halt_node(GameBackend),
                                  wait_seconds(10), % wait for resurrection

                                  [ExpectedGameBackend] = lists:delete(GameBackend, Backends),
                                  NewGameBackend = find_game_backend([ExpectedGameBackend], GameId),

                                  %% This means it has been moved.
                                  ?assertEqual(ExpectedGameBackend, NewGameBackend),

                                  %% --- LOGIN ---
                                  test_client:close(),
                                  {ok, _} = test_client:connect(WebAddr, 8000),
                                  SessId2 = login(),

                                  %% --- RECOVERY CHECK START ---
                                  ?debugMsg("RECOVERY CHECK START - GameStateStart:"),
                                  GameStateStart = get_game_state(SessId2, GameIdBin),
                                  prettyp_game_state(GameStateStart),

                                  %% --- WAIT FOR PHASE CHANGE ---
                                  wait_seconds(65),

                                  %% --- RECOVERY CHECK END ---
                                  ?debugMsg("RECOVERY CHECK END - GameStateEnd"),
                                  GameStateEnd = get_game_state(SessId2, GameIdBin),
                                  prettyp_game_state(GameStateEnd),

                                  %% just for tester
                                  ?debugVal((GameStateStart == GameStateEnd)),
                                  ?assertNot(GameStateStart == GameStateEnd)
                          end}}
                end %% instantiator
        end, %% instantiator maker
    lists:map(InstantiatorMaker, GameStates).


%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

get_web_addr(ClustConf) ->
    StartupOrder = cluster_utils:generate_startup_order(ClustConf),
    [WebHost] = [Host || {Host, _SysManPfx, Rel, _RelPfx} <- StartupOrder,
                         Rel == web_frontend],
    {host, WebHost, _SysManPfx, WebHostConf} =
        lists:keyfind(WebHost, 2, ClustConf),
    {release, web_frontend, _RelPfx, WebFERelConf} =
        lists:keyfind(web_frontend, 2, WebHostConf),
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
    case is_push_msg(ReplyTerm) of
        true ->
            ?debugMsg("Skipping pushed message."),
            web_frontend_recv();
        false ->
            ReplyTerm
    end.

login() ->
    web_frontend_send(?JSON_LOGIN),
    LoginReplyTerm = web_frontend_recv(),
    SessId = event_login_get_session(LoginReplyTerm),
    ?debugVal(SessId),
    SessId.

get_game_state(SessId, GameIdBin) ->
    web_frontend_send(?JSON_GAME_OVERVIEW(SessId, GameIdBin)),
    GameOverviewReply = web_frontend_recv(),
    GameState = event_gameoverview_get_game_state(GameOverviewReply),
    GameState.

prettyp_game_state({game_state, Year, Season, Phase}) ->
    ?debugFmt("~n~n= GAME INFO: =====~n"
              "Year ~s~nSeason ~s~nPhase ~s~n",
              [Year, Season, Phase]).

wait_seconds(Seconds) ->
    ?debugFmt("Waiting ~p seconds...~n", [Seconds]),
    timer:sleep(1000 * Seconds).

halt_node(Node) ->
    ?debugFmt("Halting ~p~n", [Node]),
    %% it should be up before we halt it for correctness
    pong = net_adm:ping(Node),
    %% it should be down after we halt it
    {badrpc,nodedown} = rpc:call(Node, erlang, halt, []).

advance_to_state(Backend, GameId, Season, Phase) ->
    {ok, _NewPhase} =
        rpc:call(Backend, game_timer, sync_event, [GameId, timeout]),
    case rpc:call(Backend, game, get_current_game, [GameId]) of
        {ok, #game_current{ year_season={_,Season}, current_phase=Phase}} ->
            ok;
        {ok, _} ->
            advance_to_state(Backend, GameId, Season, Phase)
    end.

find_game_backend([], _) -> {error, corpse_not_found};
find_game_backend([Backend | Rest], GameId) ->
    Corpses = rpc:call(Backend, corpses, get_corpses, [Backend]),
    case [GameId || {game_timer, {_, CorpseGameId}} <- Corpses,
                    CorpseGameId == GameId] of
        [] ->
            find_game_backend(Rest, GameId);
        [GameId] ->
            Backend
    end.


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
    {match, [GameIdBin]} =
        re:run(SuccessMsg, "\"(.*)\"", [{capture, all_but_first, binary}]),
    GameIdBin.

event_gameoverview_get_game_state(JSONTerm) ->
    {struct,[{<<"event">>,<<"game_overview_success">>},
             {<<"event_data">>, {struct, EventData}},
             _,_,_
            ]} = JSONTerm,
    {<<"year">>, Year} =
        lists:keyfind(<<"year">>, 1, EventData),
    {<<"season">>, Season} =
        lists:keyfind(<<"season">>, 1, EventData),
    {<<"phase">>, Phase} =
        lists:keyfind(<<"phase">>, 1, EventData),
    {game_state, Year, Season, Phase}.

is_push_msg(JSONTerm) ->
    {struct, KeyPairs} = JSONTerm,
    case lists:keyfind(<<"event">>, 1, KeyPairs) of
        %% Could add future push types here
        {<<"event">>,<<"phase_change_ok">>} ->
            true;
        _ ->
            false
    end.
