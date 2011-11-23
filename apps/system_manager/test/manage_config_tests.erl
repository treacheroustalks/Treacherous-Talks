-module(manage_config_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORIGINAL_CONFIG_PATH, "test_app.config").
-define(NEW_CONFIG_PATH, "test_app-override.config").

-define(ORIGINAL_CONFIG,
        [{user_management,[{user_management_workers,1}]},
         {db,[{riak,{pb,{"127.0.0.1",8081}}},{db_workers,50}]},
         {game,[{game_workers,1}]}
        ]).
-define(CONFIG_MODIFICATIONS,
        [{db,[{riak,{pb,{"192.168.0.2",8081}}},{db_workers,50}]},
         {smtp, [{hostname, mail.pcs}]}
        ]).
-define(MODIFIED_CONFIG,
        [{user_management,[{user_management_workers,1}]},
         {db,[{riak,{pb,{"192.168.0.2",8081}}},{db_workers,50}]},
         {game,[{game_workers,1}]},
         {smtp, [{hostname, mail.pcs}]}
        ]).

manage_config_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [fun read_config_tst/0,
      fun update_config_tst/0,
      fun write_config_tst/0,
      fun get_write_get_tst/0
     ]}.

setup() ->
     OriginalConfigString = lists:flatten(
                              io_lib:format("~p.",
                                            [?ORIGINAL_CONFIG])),
     ok = file:write_file(?ORIGINAL_CONFIG_PATH, OriginalConfigString).

teardown(_) ->
    ?cmd("rm -f " ++ ?ORIGINAL_CONFIG_PATH),
    ?cmd("rm -f " ++ ?NEW_CONFIG_PATH).

read_config_tst() ->
    {ok, Config} = manage_config:read_config(?ORIGINAL_CONFIG_PATH),
    ?assertEqual(?ORIGINAL_CONFIG, Config).

update_config_tst() ->
    NewConfig = manage_config:update_config(?ORIGINAL_CONFIG,
                                            ?CONFIG_MODIFICATIONS),
    ?assertEqual(?MODIFIED_CONFIG, NewConfig).

write_config_tst() ->
    NewConfig = manage_config:update_config(?ORIGINAL_CONFIG,
                                            ?CONFIG_MODIFICATIONS),
    ok = manage_config:write_config(?NEW_CONFIG_PATH, NewConfig),
    ExpectedConfigString = lists:flatten(
                             io_lib:format("~p.",
                                           [?MODIFIED_CONFIG])),
    {ok, ActualConfigBin} = file:read_file(?NEW_CONFIG_PATH),
    ActualConfigString = binary_to_list(ActualConfigBin),
    ?assertEqual(ExpectedConfigString, ActualConfigString).

get_write_get_tst() ->
    {ok, Get1Val} = manage_config:read_config(?ORIGINAL_CONFIG_PATH),
    ok = manage_config:write_config(?ORIGINAL_CONFIG_PATH, Get1Val),
    {ok, Get2Val} = manage_config:read_config(?ORIGINAL_CONFIG_PATH),
    ?assertEqual(Get1Val, Get2Val).
