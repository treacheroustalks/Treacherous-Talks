%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
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
-include_lib("datatypes/include/push_receiver.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").
-include_lib("datatypes/include/bucket.hrl").


-export([create_user/0, create_game/0, get_receiver/0, get_event/0,
        create_operator/0]).

-define(TIMEOUT, 3000).
%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [mnesia, service, datatypes, protobuffs, riakc, db, utils, necromancer,
     message, user_management, game, controller_app].

app_start() ->
    lists:foreach (fun (App) ->
                           case application:start (App) of
                               {error, {already_started, App}} -> ok;
                               ok -> ok;
                               Other ->
                                   erlang:error ({error,
                                                  {?MODULE, ?LINE,
                                                   'could not start',
                                                   App,
                                                   'reason was', Other}})
                           end
%                           ?debugMsg (
%                              io_lib:format ("~p is running", [App]))
                   end,
                   apps ()),
    error_logger:tty(false),
    register(receiver, spawn(fun() -> receiver([]) end)).

app_stop(_) ->
    whereis(receiver) ! stop,
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
%% helper: event receiver
%%-------------------------------------------------------------------
receiver(Events) ->
   receive
       {push, no_args, Event} ->
           receiver([Event|Events]);
       {get_events, Pid} ->
           Pid ! Events,
           receiver([]);
       stop ->
           ok;
       Other ->
           ?debugMsg("Receiver received unknown message!"),
           ?debugVal(Other),
           receiver(Events)
   end.


get_receiver() ->
    #push_receiver{pid = whereis(receiver),
                   args = no_args,
                   type = default
                  }.

get_event() ->
    whereis(receiver) ! {get_events, self()},
    receive
        Any -> Any
    after
        ?TIMEOUT -> {error, timeout}
    end.

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
       ?_test(invalid_session(callback()))
      ] ++
      register_tests:tests(callback()) ++
      login_tests:tests(callback()) ++
      assign_moderator_tests:tests(callback())
     }}.

controller_session_test_() ->
    {setup,
     fun() ->
             app_start(),
             session_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

controller_multiple_session_test_() ->
    {setup,
     fun() ->
             app_start(),
             multiple_session_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

controller_operator_session_test_() ->
    {setup,
     fun() ->
             app_start(),
             operator_session_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

controller_moderator_session_game_test_() ->
    {setup,
     fun() ->
             app_start(),
             moderator_session_joined_game_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

controller_pre_game_test_() ->
    {setup,
     fun() ->
             app_start(),
             pre_game_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

controller_joined_game_test_() ->
    {setup,
     fun() ->
             app_start(),
             joined_game_setup()
     end,
     fun app_stop/1,
     fun instantiator/1
    }.

register_oprator_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [register_oprator_tst_()]
     }.

%%-------------------------------------------------------------------
%% Test instantiator
%%-------------------------------------------------------------------
instantiator({Mods, Args}) ->
    lists:flatten(
      lists:map(fun(Mod) ->
                        Mod:tests(Args)
                end, Mods));
instantiator(ModArgs) when is_list(ModArgs) ->
    lists:flatten(
      lists:map(fun({Mod, Args}) ->
                        Mod:tests(Args)
                end, ModArgs)).

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
                create_game, get_game, reconfig_game, game_overview,
                join_game, game_order, assign_moderator, power_msg,
                stop_game
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
                update_user, get_session_user, create_game, get_game,
                reconfig_game, join_game, game_order, assign_moderator,
                power_msg, stop_game
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
            create_game_tests, user_msg_tests,
            push_events_tests, logout_tests
           ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, get_receiver()}}},
    SessId = controller:handle_action(Login, Reply),

    {Mods, [Callback, SessId]}.

%%-------------------------------------------------------------------
%% Multiple sessions tests
%% Creates multiple users (online and offline) available for tests
%%-------------------------------------------------------------------
multiple_session_setup() ->
    %% Config = [{test_module_name, OnlineUserCount, OfflineUserCount}]
    Configs = [
               {get_presence_tests, 2, 1}
              ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    CreateUser= fun() ->
                        User = create_user(),
                        Register = {register, {ok, User}},
                        controller:handle_action(Register, Reply)
                end,
    LoginUser = fun(NewUser) ->
                        Login = {login, {ok, {NewUser, get_receiver()}}},
                        controller:handle_action(Login, Reply)
                end,

    ModUsers = lists:map(
                 fun({Mod, OnlineCount, OfflineCount}) ->
                         OnlineUsers =
                             lists:map(
                               fun(_) ->
                                       NewUser = CreateUser(),
                                       LoginUser(NewUser),
                                       NewUser#user.id
                               end, lists:seq(1, OnlineCount)),
                         OfflineUsers =
                             lists:map(
                               fun(_) ->
                                       NewUser = CreateUser(),
                                       NewUser#user.id
                               end, lists:seq(1, OfflineCount)),
                         {Mod, OnlineUsers, OfflineUsers} end,
                 Configs),
    lists:map(fun({Mod, OnlineUsers, OfflineUsers}) ->
                      {Mod, [Callback, OnlineUsers, OfflineUsers]}
              end, ModUsers).

%%-------------------------------------------------------------------
%% Operator tests
%%-------------------------------------------------------------------
operator_session_setup() ->
    Mods = [
            stop_game_tests, get_db_stats_tests, get_games_ongoing_tests
           ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    User = create_user(),
    Operator = User#user{role = operator},
    Register = {register, {ok, Operator}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, get_receiver()}}},
    SessId = controller:handle_action(Login, Reply),

    lists:map(fun(Mod) ->
                      {Mod, Callback, SessId}
              end, Mods),
    {Mods, [Callback, SessId]}.
%%-------------------------------------------------------------------
%%  Register operator test
%%-------------------------------------------------------------------
register_oprator_tst_() ->
    [{"test register_oprator/1 when get user record as input",
      fun() ->
            User = create_user(),
            {ok, Result}= controller:register_operator(User),
            ?assertEqual(operator, Result#user.role)
    end},
     {"test register_oprator/2",
      fun() ->
            {ok, Result}= controller:register_operator("operatorNick","password"),
            ok = db:delete(?B_USER, db:int_to_bin(Result#user.id)),
            ?assertEqual(operator, Result#user.role),
            ?assertEqual("operatorNick", Result#user.nick)
    end}
    ].

%%-------------------------------------------------------------------
%% Pre-Game tests
%%-------------------------------------------------------------------
pre_game_setup() ->
    Mods = [
            reconfig_game_tests, join_game_tests, get_game_tests,
            game_search_tests
           ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, get_receiver()}}},
    SessId = controller:handle_action(Login, Reply),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate, Reply),

    {Mods, [Callback, SessId, GameId]}.

%%-------------------------------------------------------------------
%% Joined game tests
%%-------------------------------------------------------------------
joined_game_setup() ->
    Mods = [
            games_current_tests, game_overview_tests, game_order_tests
           ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, get_receiver()}}},
    SessId = controller:handle_action(Login, Reply),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate, Reply),

    JoinGame = {join_game, {ok, SessId, {GameId, germany}}},
    controller:handle_action(JoinGame, Reply),

    {Mods, [Callback, SessId, GameId]}.

%%-------------------------------------------------------------------
%% Moderator with game tests
%%-------------------------------------------------------------------
moderator_session_joined_game_setup() ->
    Mods = [
            power_msg_tests
           ],
    Callback = callback(),
    Reply = {fun(_,_,Data) -> Data end, []},

    % create moderator
    ModUser = create_user(),
    Moderator = ModUser#user{role = moderator},
    RegisterMod = {register, {ok, Moderator}},
    NewUserMod = controller:handle_action(RegisterMod, Reply),

    LoginMod = {login, {ok, {NewUserMod, get_receiver()}}},
    SessIdMod = controller:handle_action(LoginMod, Reply),

    % create gameplayer and join game
    User = create_user(),
    Register = {register, {ok, User}},
    NewUser= controller:handle_action(Register, Reply),

    Login = {login, {ok, {NewUser, get_receiver()}}},
    SessId = controller:handle_action(Login, Reply),

    NewGame = create_game(),
    GameCreate = {create_game, {ok, SessId, NewGame}},
    GameId = controller:handle_action(GameCreate, Reply),

    JoinGame = {join_game, {ok, SessId, {GameId, germany}}},
    controller:handle_action(JoinGame, Reply),

    {Mods, [Callback, SessIdMod, GameId, germany]}.

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
          waiting_time = 1}.

create_operator() ->
    #user{id = undefined,
          nick = "testuser" ++ integer_to_list(db_c:get_unique_id()),
          email = "test@user.com",
          password = "test_passw0rd",
          name = "Test User",
          role = operator,
          channel = mail,
          last_ip = {127, 0, 0, 0},
          last_login = never,
          score = 0,
          date_created = {{2011, 10, 18}, {10, 42, 15}},
          date_updated = {{2011, 10, 18}, {10, 42, 16}}}.
