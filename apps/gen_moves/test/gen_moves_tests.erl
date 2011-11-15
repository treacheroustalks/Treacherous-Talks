-module (gen_moves_tests).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("datatypes/include/user.hrl").
-include_lib ("datatypes/include/game.hrl").

test_user () ->
    #user{nick = "gen_moves_user",
          email = "gen_moves@gmail.com",
          password = "gen_moves",
          name = "Gen. Howard K. Moves",
          role = user,
          channel = mail,
          last_ip = {0,0,0,0},
          last_login = never}.

test_game () ->
    #game{name = "gen_moves test-game",
           press = white_press,
           order_phase = 60,
           retreat_phase = 60,
           build_phase = 60,
           waiting_time = 120}.

apps() ->
    [protobuffs, riakc, service, db, datatypes, user_management,
     game, controller_app].

run_apps_setup() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

run_apps_teardown(_) ->
    error_logger:tty(false),
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())].

create_context_setup () ->
    %% the context: test user
    %% is created and plays as england in the test game
    % create the test user, if he exists, 'get' him:
    User = case controller:register (TestU = test_user ()) of
               {ok, U} ->
                   U;
               {error, nick_already_exists} ->
                   TestNick = TestU#user.nick,
                   [U] = user_management:get (#user.nick, TestNick),
                   U
           end,
    ?debugVal (User),
    SessId = controller:handle_action (
               {login, {ok, User}},
               {fun (_, _, Data) -> Data end, []}),
    ?debugVal (SessId),
    GameId = controller:handle_action (
               {create_game, {ok, SessId, test_game ()}},
               {fun (_, _, Data) -> Data end, []}),
    controller:handle_action (
      {join_game, {ok, SessId, {GameId, england}}},
      {fun (_, _, Data) -> Data end, []}),

    {SessId, GameId}.

create_context_teardown ({_SessId, _GameId}) ->
    ok.

%% since the output is supposed to be random and valid order syntax, we cannot
%% test much here..
%% the validity of the syntax is ensured because the orders are run through
%% `rules:process/4' after they were created and that function crashes on
%% illegal syntax.
main_test_ () ->
    {setup,
     fun run_apps_setup/0,
     fun run_apps_teardown/1,
     {foreach,
      fun create_context_setup/0,
      fun create_context_teardown/1,
      [fun gen_moves_tst_/1,
       fun parse_orders_tst_/1]}}.

gen_moves_tst_ ({SessId, GameId}) ->
    fun () ->
            Dicts = gen_moves:generate_orders (
                      map_data:create (
                        standard_game),
                      SessId,
                      GameId),
            lists:foreach (fun (Dict) ->
                                   ?debugVal (dict:to_list (Dict))
                           end,
                           Dicts)
    end.

parse_orders_tst_ ({_SessId, _GameId}) ->
    fun () ->
            ok
    end.
