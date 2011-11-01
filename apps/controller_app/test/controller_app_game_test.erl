%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for controller_app
%%% @end
%%%
%%% @since : 31 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller_app_game_test).

-define(TIMEOUT, 3000).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Runs all tests
%% @end
%%-------------------------------------------------------------------
controller_game_test_() ->
    {inorder,
     [
      fun setup/0,
      fun controller_new_game/0,
      fun controller_handle_action_create_game_success/0,
      fun controller_handle_action_create_game_error/0,
      fun controller_handle_reconfig_game_success/0,
      fun controller_handle_reconfig_game_invalid/0,
      fun controller_handle_reconfig_game_error/0,
      fun controller_handle_update_game_invalid_session/0,
      fun controller_handle_update_game_invalid_gamestate_session/0,
      fun controller_handle_game_overview_success/0,
      fun controller_handle_game_overview_invalid_session/0,
      fun controller_handle_game_overview_parse_error/0,
      fun controller_handle_join_game_success/0,
      fun controller_handle_join_game_invalid_session/0,
      fun controller_handle_join_game_error/0,
      fun controller_handle_join_game_parse_error/0,
      fun teardown/0
     ]}.

%% @doc
%% Tests the handle_action functionality in the controller for
%% successful game creation.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_create_game_success() ->
    ?debugMsg("Testing handle_action: create_game success"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),
    meck:expect(controller, new_game, 1, return_value),

    SessionId = 123456,
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    Game = get_new_test_game(),
    Result = controller:handle_action(
                               {create_game, {ok, SessionId, Game}},
                               {Callback, []}),
    ?assertEqual({{create_game, success}, return_value}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: create_game success").


%% @doc
%% Tests the handle_action functionality in the controller for
%% parse_error during game_create.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_create_game_error() ->
    ?debugMsg("Testing handle_action: create_game parse error"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {create_game, error},
                               {Callback, []}),
    ?assertEqual({create_game, parse_error}, Result),

    ?debugVal("Completed handle_action: create_game parse error").

%%-------------------------------------------------------------------
%% @doc
%% Tests updating of new game
%%
%% The session is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_handle_reconfig_game_success() ->
    ?debugMsg("Testing handle_action: reconfig_game success"),
    meck:new(controller, [passthrough]),
    SessionId = 123456,
    Game = get_test_game(SessionId),
    User = #user{id = SessionId},

    meck:expect(controller, get_game, 1, {ok, Game}),
    meck:expect(controller, get_session_user, 1, {ok, User}),
    meck:expect(controller, reconfig_game, 1, {ok, 666}),

    GameProplist = [{#game.name, "NEW NAME"}],
    NewGame = Game#game{name = "NEW NAME"},
    Callback = fun([], Result, Data) -> {Result, Data} end,
    Result = controller:handle_action(
                            {reconfig_game, {ok, SessionId, 1, GameProplist}},
                            {Callback, []}),
    ?assertEqual({{reconfig_game, success}, NewGame}, Result),
    meck:unload(controller),
    ?debugVal("Completed handle_action: reconfig_game success").

%%-------------------------------------------------------------------
%% @doc
%% Tests updating of new game, when it is invalid
%% example of invalid update is when the game does not have
%% status =:= waiting
%%
%% The session is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_handle_reconfig_game_invalid() ->
    ?debugMsg("Testing handle_action: reconfig_game invalid"),
    meck:new(controller, [passthrough]),
    SessionId = 123456,
    Game = get_test_game(SessionId),
    NotInWaitingGame = Game#game{status = finished},
    meck:expect(controller, get_game, 1, {ok, NotInWaitingGame}),
    meck:expect(controller, reconfig_game, 1, {ok, 666}),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),
    PropListGame = [{#game.name, "NEW NAME"}],
    Callback = fun([], Result, Data) -> {Result, Data} end,
    Result = controller:handle_action(
                            {reconfig_game, {ok, SessionId, 1, PropListGame}},
                            {Callback, []}),
    ?assertEqual({{reconfig_game, invalid_data}, PropListGame}, Result),
    meck:unload(controller),
    ?debugVal("Completed handle_action: reconfig_game invalid").

%%-------------------------------------------------------------------
%% @doc
%% Tests updating of new game, for the parse_error case
%%
%% The session is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_handle_reconfig_game_error() ->
    ?debugMsg("Testing handle_action: reconfig_game parse error"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {reconfig_game, error},
                               {Callback, []}),
    ?assertEqual({reconfig_game, parse_error}, Result),
    ?debugMsg("Completed handle_actio: reconfig_game parse error").

%%-------------------------------------------------------------------
%% @doc
%% Tests creation of new game
%%
%% The session is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_new_game() ->
    ?debugVal("Testing new game"),
    fake_game_setup(),
    Game={game, 1234, not_a_real_game},
    game:new_game({self(), new_game}, Game),
    receive
        {new_game, {was_called, new_game}} ->
            ok;
                Other ->
                ?debugMsg(io_lib:format("received ~p, expected ~p",
                [Other, {new_game,
                {was_called, new_game}}]))
        after ?TIMEOUT ->
            ?debugMsg("ERROR: received no answer from fake fun."),
            erlang:error({error, received_no_answer})
        end,
    fake_game_teardown(),
    ?debugVal("New game testing complete").


%%-------------------------------------------------------------------
%% @doc
%% Tests updating of a game with an invalid session
%% @end
%%-------------------------------------------------------------------
controller_handle_update_game_invalid_session() ->
    ?debugMsg("Testing updating game with invalid session"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {error, msg}),
    SessionId = 123456,
    Game = get_test_game(),
    ChangeList = [{#game.description, "Failing tests"}],
    UpdateResult = controller:handle_action(
                                {reconfig_game, {ok, SessionId,
                                                 Game#game.id, ChangeList}},
                                {Callback, []}),

    meck:unload(controller),
    ?assertEqual({{reconfig_game, invalid_session}, SessionId}, UpdateResult),
    ?debugMsg("Testing updating game with invalid session end").

%%-------------------------------------------------------------------
%% @doc
%% Tests updating of a game in an invalid gamestate, but with a
%% valid session
%% @end
%%-------------------------------------------------------------------
controller_handle_update_game_invalid_gamestate_session() ->
    ?debugMsg("Testing updating game with invalid gamestate valid session"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    User = get_registered_user(),
    Game = get_test_game(User#user.id),
    NotInWaitingGame = Game#game{status = finished},
    SessionId = 123456789,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_game, 1, {ok, NotInWaitingGame}),
    meck:expect(controller, get_session_user, 1, {ok, User}),
    ChangeList = [{#game.description, "New description!"}],
    UpdateResult = controller:handle_action(
                     {reconfig_game, {ok, SessionId,
                                      NotInWaitingGame#game.id, ChangeList}},
                     {Callback, []}),
    ?assertEqual({{reconfig_game, invalid_data}, ChangeList}, UpdateResult),
    meck:unload(controller),
    ?debugMsg("Testing updating game with invalid gamestate valid session end").

%%-------------------------------------------------------------------
%% @doc
%% Tests getting a game overview
%% @end
%%-------------------------------------------------------------------
controller_handle_game_overview_success() ->
    ?debugMsg("Testing game overview success"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    User = get_registered_user(),
    SessionId = 123456789,
    GameId = 1234,
    meck:new(controller, [passthrough]),
    meck:new(game),
    meck:expect(controller, get_session_user, 1, {ok, User}),
    meck:expect(controller, game_overview, 2, game_info),
    Result = controller:handle_action(
               {game_overview, {ok, SessionId, GameId}},
               {Callback, []}),
    ?assertEqual({{game_overview, success}, game_info}, Result),
    meck:unload(controller),
    meck:unload(game),
    ?debugMsg("Tesing game overview success end").

controller_handle_game_overview_invalid_session() ->
    ?debugMsg("Testing game overview invalid session"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    SessionId = 123456789,
    GameId = 1234,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {error, SessionId}),
    Result = controller:handle_action(
               {game_overview, {ok, SessionId, GameId}},
               {Callback, []}),
    ?assertEqual({{game_overview, invalid_session}, SessionId}, Result),
    meck:unload(controller),
    ?debugMsg("Tesing game overview invalid session end").

controller_handle_game_overview_parse_error() ->
    ?debugMsg("Testing game overview parse error"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    Result = controller:handle_action({game_overview, error}, {Callback, []}),
    ?assertEqual({{game_overview, parse_error}, error}, Result),
    ?debugMsg("Testing game overview parse error end").


%%-------------------------------------------------------------------
%% @doc
%% Tests joining a game
%% @end
%%-------------------------------------------------------------------
controller_handle_join_game_success() ->
    ?debugMsg("Testing join game success"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),
    meck:expect(controller, join_game, 3, {ok, 111222}),
    Session = 12345678,
    GameId = 111222,
    Result = controller:handle_action(
               {join_game, {ok, Session, GameId, some_country}},
               {Callback, []}),
    ?assertEqual({{join_game, success}, 111222}, Result),
    meck:unload(controller),
    ?debugMsg("Testing join game success end").
    
controller_handle_join_game_invalid_session() ->
    ?debugMsg("Testing join game invalid session"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {error, some_error}),
    Session = 12345678,
    GameId = 111222,
    Result = controller:handle_action(
               {join_game, {ok, Session, GameId, some_country}},
               {Callback, []}),
    ?assertEqual({{join_game, invalid_session}, Session}, Result),
    meck:unload(controller),
    ?debugMsg("Testing join game invalid session end").

controller_handle_join_game_error() ->
    ?debugMsg("Testing join game error - could not join"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),
    meck:expect(controller, join_game, 3, {error, could_not_join}),
    Session = 12345678,
    GameId = 111222,
    Result = controller:handle_action(
               {join_game, {ok, Session, GameId, some_country}},
               {Callback, []}),
    ?assertEqual({{join_game, error}, could_not_join}, Result),
    meck:unload(controller),
    ?debugMsg("Testing join game error - could not join end").

controller_handle_join_game_parse_error() ->
    ?debugMsg("Testing join game parse error"),
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    Result = controller:handle_action({join_game, error}, {Callback, []}),
    ?assertEqual({{join_game, parse_error}, error}, Result),
    ?debugMsg("Testing join game parse error end").


%%-------------------------------------------------------------------
%% @doc
%% Sets up the tests
%% @end
%%-------------------------------------------------------------------
setup() ->
    error_logger:tty(false),
    ?debugVal("Starting ALL GAME tests"),
    % Application has to be loaded to get env variables
    application:load(controller_app),
    ?debugVal(application:start(protobuffs)),
    ?debugVal(application:start(riakc)),
    ?debugVal(application:start(db)),
    ?debugVal(controller_app_sup:start_link()).


%%-------------------------------------------------------------------
%% @doc
%% Cleanup tests
%% @end
%%-------------------------------------------------------------------
teardown() ->
    ?debugVal("Completing ALL GAME tests").


%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------
fake_game_setup() ->
    meck:new(game),
    meck:expect(game, new_game,
                fun(From, _Game) ->
                        gen_server:reply(From, {was_called, new_game})
                end).


fake_game_teardown() ->
    meck:unload(game).

%%-------------------------------------------------------------------
%% Test data

get_test_game() ->
    get_test_game(74253467).
get_test_game(Id) ->
    #game{creator_id = Id,
          name="game name",
          description="lorem ipsum dolor sit amet",
          press = black_press,
          order_phase = 12*60,
          retreat_phase = 12*60,
          build_phase = 12*60,
          password="pass",
          waiting_time = 48*60}.

get_new_test_game () ->
    #game{name="game name",
          description="lorem ipsum dolor sit amet",
          press = black_press,
          order_phase = 12*60,
          retreat_phase = 12*60,
          build_phase = 12*60,
          password="pass",
          waiting_time = 48*60}.

get_registered_user() ->
    #user{id = 74253467,
          nick = "uniq",
          email = "she@mail.com",
          password = "secret",
          name = "Unique Monique",
          role = undefined,
          channel = undefined,
          last_ip = undefined,
          last_login = undefined,
          score = 0,
          date_created = undefined,
          date_updated = undefined}.
