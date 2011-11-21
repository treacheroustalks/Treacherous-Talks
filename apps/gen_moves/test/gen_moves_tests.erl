-module (gen_moves_tests).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/user.hrl").
-include_lib ("datatypes/include/game.hrl").

apps() ->
    [protobuffs, riakc, service, db, datatypes, user_management,
     game, controller_app].

run_apps_setup() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

run_apps_teardown(_) ->
    error_logger:tty(false),
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())].

%% since the output is supposed to be random and valid order syntax, we cannot
%% test much here..
%% the validity of the syntax is ensured because the orders are run through
%% `rules:process/4' after they were created and that function crashes on
%% illegal syntax.
main_test_ () ->
    {setup,
     fun run_apps_setup/0,
     fun run_apps_teardown/1,
     [fun gen_moves_tst_/0,
      fun parse_orders_tst_/0]}.

gen_moves_tst_ () ->
    Dicts = gen_moves:generate_orders (
              map_data:create (
                standard_game)),
    lists:foreach (fun (Dict) ->
                           ?debugVal (dict:to_list (Dict))
                   end,
                   Dicts).

parse_orders_tst_ () ->
    ok.
