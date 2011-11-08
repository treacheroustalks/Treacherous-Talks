%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for the controller interface.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

-export([create_user/0, create_game/0]).
%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [protobuffs, riakc, service, db, datatypes, user_management,
     game, controller_app].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% helper: controller callback
%%-------------------------------------------------------------------
callback(_Args, Result, Info) ->
    {Result, Info}.
callback() ->
    {fun callback/3, []}.

%%-------------------------------------------------------------------
%% All controller interface tests
%%-------------------------------------------------------------------
controller_test_() ->
    {timeout, 60,
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [
       ?_test(parse_error(callback())),
       ?_test(unknown_command(callback())),
       ?_test(invalid_session(callback())),

       ?_test(register_tests:success(callback())),
       ?_test(register_tests:invalid(callback())),

       ?_test(login_tests:success(callback())),
       ?_test(login_tests:invalid(callback()))
      ]
     }}.

controller_session_test_() ->
    {setup,
     fun() ->
             app_start(),
             session_setup()
     end,
     fun app_stop/1,
     fun session_test_instantiator/1
    }.

controller_pre_game_test_() ->
    {setup,
     fun() ->
             app_start(),
             pre_game_setup()
     end,
     fun app_stop/1,
     fun pre_game_test_instantiator/1
    }.

controller_joined_game_test_() ->
    {setup,
     fun() ->
             app_start(),
             joined_game_setup()
     end,
     fun app_stop/1,
     fun joined_game_test_instantiator/1
    }.

controller_game_order_test_() ->
    {setup,
     fun() ->
             app_start(),
             game_order_setup()
     end,
     fun app_stop/1,
     fun game_order_test_instantiator/1
    }.


%%-------------------------------------------------------------------
%% Unknown command tests
%%-------------------------------------------------------------------
unknown_command(Callback) ->
    Command = unknown_command,
    Result = controller:handle_action(Command, Callback),
    {CmdRes, _Info} = Result,
    ?assertEqual(unknown_command, CmdRes).

%%-------------------------------------------------------------------
%% Parse error tests
%%-------------------------------------------------------------------
parse_error(Callback) ->
    Commands = [
                login, register, update_user, get_session_user,
                create_game, reconfig_game, game_overview,
                join_game, game_order
               ],

    lists:foreach(fun(Cmd) ->
                          Command = {Cmd, {error, an_error}},
                          Result = controller:handle_action(Command, Callback),
                          {CmdRes, _Info} = Result,
                          ?assertEqual({Cmd, parse_error}, CmdRes)
                  end, Commands).

%%-------------------------------------------------------------------
%% Invalid session tests
%%-------------------------------------------------------------------
invalid_session(Callback) ->
    Commands = [
                update_user, get_session_user, create_game,
                reconfig_game, game_overview, join_game, game_order
               ],
    InvalidId = session_id:from_pid(list_to_pid("<0.1.0>")),

    lists:foreach(fun(Cmd) ->
                          Command = {Cmd, {ok, InvalidId, []}},
                          Result = controller:handle_action(Command, Callback),
                          {CmdRes, _Info} = Result,
                          ?assertEqual({Cmd, invalid_session}, CmdRes)
                  end, Commands).

%%-------------------------------------------------------------------
%% Session tests
%%-------------------------------------------------------------------
session_setup() ->
    Mods = [
            update_user_tests, get_session_user_tests,
            create_game_tests
           ],
    Callback = callback(),

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register,
                                      {fun(_,_,Data) -> Data end, []}),

    Login = {login, {ok, NewUser}},
    SessId = controller:handle_action(Login,
                                      {fun(_,_,Data) -> Data end, []}),

    lists:map(fun(Mod) ->
                      {Mod, Callback, SessId}
              end, Mods).

session_test_instantiator(Mods) ->
    lists:flatten(
      lists:map(fun({Mod, Callback, SessId}) ->
                        fun() ->
                                [Mod:success(Callback, SessId),
                                 Mod:invalid(Callback, SessId)]
                        end
                end, Mods)).

%%-------------------------------------------------------------------
%% Pre-Game tests
%%-------------------------------------------------------------------
pre_game_setup() ->
    Mods = [
            reconfig_game_tests, join_game_tests
           ],
    Callback = callback(),

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register,
                                      {fun(_,_,Data) -> Data end, []}),

    Login = {login, {ok, NewUser}},
    SessId = controller:handle_action(Login,
                                      {fun(_,_,Data) -> Data end, []}),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate,
                                      {fun(_,_,Data) -> Data end, []}),

    lists:map(fun(Mod) ->
                      {Mod, Callback, SessId, GameId}
              end, Mods).

pre_game_test_instantiator(Mods) ->
    lists:flatten(
      lists:map(fun({Mod, Callback, SessId, GameId}) ->
                        fun() ->
                                [Mod:success(Callback, SessId, GameId),
                                 Mod:invalid(Callback, SessId, GameId)]
                        end
                end, Mods)).

%%-------------------------------------------------------------------
%% Joined game tests
%%-------------------------------------------------------------------
joined_game_setup() ->
    Mods = [
            game_overview_tests
           ],
    Callback = callback(),

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register,
                                      {fun(_,_,Data) -> Data end, []}),

    Login = {login, {ok, NewUser}},
    SessId = controller:handle_action(Login,
                                      {fun(_,_,Data) -> Data end, []}),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate,
                                      {fun(_,_,Data) -> Data end, []}),

    JoinGame = {join_game, {ok, SessId, {GameId, germany}}},
    controller:handle_action(JoinGame,
                             {fun(_,_,Data) -> Data end, []}),

    lists:map(fun(Mod) ->
                      {Mod, Callback, SessId, GameId}
              end, Mods).

joined_game_test_instantiator(Mods) ->
    lists:flatten(
      lists:map(fun({Mod, Callback, SessId, GameId}) ->
                        fun() ->
                                [Mod:success(Callback, SessId, GameId),
                                 Mod:invalid(Callback, SessId, GameId)]
                        end
                end, Mods)).

%%-------------------------------------------------------------------
%% game_order tests
%%-------------------------------------------------------------------


game_order_setup() ->
    Mods = [game_order_tests],
    Callback = callback(),

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register,
                                      {fun(_,_,Data) -> Data end, []}),

    Login = {login, {ok, NewUser}},
    SessId = controller:handle_action(Login,
                                      {fun(_,_,Data) -> Data end, []}),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate,
                                      {fun(_,_,Data) -> Data end, []}),

    JoinGame = {join_game, {ok, SessId, {GameId, germany}}},
    controller:handle_action(JoinGame,
                             {fun(_,_,Data) -> Data end, []}),
    lists:map(fun(Mod) ->
                      {Mod, Callback, SessId, GameId}
              end, Mods).

game_order_test_instantiator(Mods) ->
    lists:flatten(
      lists:map(fun({Mod, Callback, SessId, GameId}) ->
                        fun() ->
                                [Mod:success(Callback, SessId, GameId),
                                 Mod:invalid(Callback, SessId, GameId)]
                        end
                end, Mods)).


%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
create_user() ->
    #user{id = undefined,
          nick = "testuser" ++ integer_to_list(db_c:get_unique_id()),
          email = "test@user.com",
          password = "test_passw0rd",
          name = "Test User",
          role = user,
          channel = mail,
          last_ip = {127, 0, 0, 0},
          last_login = never,
          score = 0,
          date_created = {{2011, 10, 18}, {10, 42, 15}},
          date_updated = {{2011, 10, 18}, {10, 42, 16}}}.

create_game() ->
    #game{name="game name" ++ integer_to_list(db_c:get_unique_id()),
          description="lorem ipsum dolor sit amet",
          press = black_press,
          order_phase = 12*60,
          retreat_phase = 12*60,
          build_phase = 12*60,
          password="pass",
          waiting_time = 48*60}.

