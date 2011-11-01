%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for controller_app
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller_app_test).

-define(TIMEOUT, 3000).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Runs all tests
%% @end
%%-------------------------------------------------------------------
controller_test_() ->
    {inorder,
     [
      fun setup/0,
      fun controller_create_user/0,
      fun controller_login_user/0,
      fun controller_update_user/0,
      fun controller_get_user1/0,
      fun controller_get_user2/0,
      fun controller_handle_action_register_success/0,
      fun controller_handle_action_register_invalid/0,
      fun controller_handle_action_register_error/0,
      fun controller_handle_action_login_success/0,
      fun controller_handle_action_login_invalid/0,
      fun controller_handle_action_login_error/0,
      fun controller_handle_action_get_session_user_success/0,
      fun controller_handle_action_get_session_user_invalid/0,
      fun controller_handle_action_get_session_user_error/0,
      fun controller_handle_action_update_user_success/0,
      fun controller_handle_action_update_user_invalid/0,
      fun controller_handle_action_update_user_error/0,
      fun controller_handle_action_unknown_command/0,
      fun test_login_session/0,
      fun teardown/0
     ]}.


%%-------------------------------------------------------------------
%% @doc
%% Tests the create_user functionality in the controller
%%
%% The user_management_app_worker is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_create_user() ->
    ?debugVal("Testing creation of a user"),
    setup_meck(),
    {_Key, User} = get_test_data(create_user),
    meck:expect(user_management, create,
                fun(_From, _User) -> ok end),
    meck:expect(controller_app_worker, handle_call,
                fun({create_user, Usr}, From, State) ->
                        user_management:create(From, Usr),
                        {reply, From, State} end),
    {From, _Ref} = controller:create_user(User),
    ?assertEqual(self(), From),
    teardown_meck(),
    ?debugVal("Completed user creation test").


%%-------------------------------------------------------------------
%% @doc
%% Tests the update_user functionality in the controller
%%
%% The user_management_app_worker is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_update_user() ->
    ?debugVal("Testing updating a user"),
    setup_meck(),
    {User} = get_test_data(update_user),
    meck:expect(user_management, update,
                fun(_From, _User) -> ok end),
    meck:expect(controller_app_worker, handle_call,
                fun({update_user, Usr}, From, State) ->
                        user_management:update(From, Usr),
                        {reply, From, State} end),
    {From, _Ref} = controller:update_user(User),
    ?assertEqual(self(), From),
    teardown_meck(),
    ?debugVal("Completed update user test").



%% @doc
%% Tests the get_user functionality in the controller
%% @end
%%-------------------------------------------------------------------
controller_get_user1() ->
    fun () ->
            ?debugMsg("Testing getting a user with key"),
            setup_meck(),
            {_Type, Key, _User} = get_test_data(get_user),
            meck:expect(user_management, get,
                        fun(_From, _Key) -> ok end),
            meck:expect(controller_app_worker, handle_call,
                        fun({get_user, _Type1, Key1}, From, State) ->
                                user_management:get(From, Key1),
                                {reply, From, State} end),
            {From, _Ref} = controller:get_user(Key),
            ?assertEqual(self(), From),
            teardown_meck(),
            ?debugVal("Completed getting user/1 test")
    end.

%% @doc
%% Tests the get_user functionality in the controller
%% @end
%%-------------------------------------------------------------------
controller_get_user2() ->
    ?debugMsg("Testing getting a user"),
    setup_meck(),
    {Type, Key, _User} = get_test_data(get_user),
    meck:expect(user_management, get,
                fun(_From, _Type, _Key) -> ok end),
    meck:expect(controller_app_worker, handle_call,
                fun({get_user, Type1, Key1}, From, State) ->
                        user_management:get(From, Type1, Key1),
                        {reply, From, State} end),
    {From, _Ref} = controller:get_user(Type, Key),
    ?assertEqual(self(), From),
    teardown_meck(),
    ?debugVal("Completed getting user/2 test").


%% @doc
%% Tests the handle_action functionality in the controller for
%% successful registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_register_success() ->
    ?debugMsg("Testing handle_action: register success"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, create_user, 1, return_value),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, return_value} = controller:handle_action(
                               {register, {ok, user_data}},
                               {Callback, []}),
    ?assertEqual({register, success}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: register success").


%% @doc
%% Tests the handle_action functionality in the controller for
%% invalid registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_register_invalid() ->
    ?debugMsg("Testing handle_action: register invalid"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, create_user, 1, error),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {register, {ok, user_data}},
                               {Callback, []}),
    ?assertEqual({register, invalid_data}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: register invalid").


%% @doc
%% Tests the handle_action functionality in the controller for
%% parse_error during registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_register_error() ->
    ?debugMsg("Testing handle_action: register parse error"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {register, error},
                               {Callback, []}),
    ?assertEqual({register, parse_error}, Result),

    ?debugVal("Completed handle_action: register parse error").


%% @doc
%% Tests the handle_action functionality in the controller for
%% successful registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_login_success() ->
    ?debugMsg("Testing handle_action: login success"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, login_user, 1, a_session_id),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, a_session_id} = controller:handle_action(
                               {login, {ok, user_data}},
                               {Callback, []}),
    ?assertEqual({login, success}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: login success").


%% @doc
%% Tests the handle_action functionality in the controller for
%% invalid login data.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_login_invalid() ->
    ?debugMsg("Testing handle_action: login invalid"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, login_user, 1, invalid),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, user_data} = controller:handle_action(
                               {login, {ok, user_data}},
                               {Callback, []}),
    ?assertEqual({login, invalid_data}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: login invalid").


%% @doc
%% Tests the handle_action functionality in the controller for
%% parse_error during registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_login_error() ->
    ?debugMsg("Testing handle_action: login parse error"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {login, error},
                               {Callback, []}),
    ?assertEqual({login, parse_error}, Result),

    ?debugMsg("Completed handle_action: login parse error").


%% @doc
%% Tests the handle_action functionality in the controller for
%% getting user data from session successfully.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_get_session_user_success() ->
    ?debugMsg("Testing handle_action: get_session_user success"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, #user{}} = controller:handle_action(
                               {get_session_user, {ok, a_session_id}},
                               {Callback, []}),
    ?assertEqual({get_session_user, success}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: get_session_user success").


%% @doc
%% Tests the handle_action functionality in the controller for
%% invalid session data.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_get_session_user_invalid() ->
    ?debugMsg("Testing handle_action: get_session_user invalid"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {error, test_error}),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, test_error} = controller:handle_action(
                               {get_session_user, {ok, a_session_id}},
                               {Callback, []}),
    ?assertEqual({get_session_user, invalid_data}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: get_session_user invalid").


%% @doc
%% Tests the handle_action functionality in the controller for
%% parse_error during registration.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_get_session_user_error() ->
    ?debugMsg("Testing handle_action: get_session_user parse error"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {get_session_user, error},
                               {Callback, []}),
    ?assertEqual({get_session_user, parse_error}, Result),

    ?debugMsg("Completed handle_action: get_session_user parse error").

%% @doc
%% Tests the handle_action functionality in the controller for
%% successful user update.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_update_user_success() ->
    ?debugMsg("Testing handle_action: update_user success"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {ok, #user{}}),
    meck:expect(controller, update_user, 1, return_value),
    meck:expect(controller, update_session_user, 2, ok),

    ParsedUser = [{#user.name, "asdf"}],
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, return_value} = controller:handle_action(
                               {update_user, {ok, 123456 ,ParsedUser}},
                               {Callback, []}),
    ?assertEqual({update_user, success}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: update_user success").


%% @doc
%% Tests the handle_action functionality in the controller for
%% invalid user update.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_update_user_invalid() ->
    ?debugMsg("Testing handle_action: update_user invalid"),
    meck:new(controller, [passthrough]),
    meck:expect(controller, get_session_user, 1, {error, msg}),

    ParsedUser = [],
    SessionId = 123456,
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    Result = controller:handle_action(
                             {update_user, {ok, SessionId, ParsedUser}},
                             {Callback, []}),
    ?assertEqual({{update_user, invalid_session}, SessionId}, Result),

    meck:unload(controller),
    ?debugVal("Completed handle_action: update_user invalid").


%% @doc
%% Tests the handle_action functionality in the controller for
%% parse_error during user_update.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_update_user_error() ->
    ?debugMsg("Testing handle_action: update_user parse error"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, error} = controller:handle_action(
                               {update_user, error},
                               {Callback, []}),
    ?assertEqual({update_user, parse_error}, Result),

    ?debugVal("Completed handle_action: update_user parse error").




%% @doc
%% Tests the handle_action functionality in the controller for
%% unknown commands.
%% @end
%%-------------------------------------------------------------------
controller_handle_action_unknown_command() ->
    ?debugMsg("Testing handle_action: unknown_command"),

    Callback = fun ([], Result, Data) -> {Result, Data} end,
    {Result, []} = controller:handle_action(
                               unknown_command,
                               {Callback, []}),
    ?assertEqual(unknown_command, Result),

    ?debugVal("Completed handle_action: create_game parse error").



%%-------------------------------------------------------------------
%% @doc
%% Tests the user login functionality in the controller
%%
%% The session is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_login_user() ->
    ?debugVal("Testing login of a user"),
    setup_meck(),
    {User, Id} = get_test_data(login),
    meck:expect(user_management, is_valid,
                fun(_Nick, _Password) -> User end),
    SessionId = controller:login_user(User),
    ?assertEqual(SessionId, Id),
    teardown_meck(),
    ?debugVal("Completed login test").



%%-------------------------------------------------------------------
%% Helper for session tests (log in)
%%-------------------------------------------------------------------
test_login_session() ->
    Callback = fun ([], Result, Data) -> {Result, Data} end,
    User = get_registered_user(),
    meck:new(user_management),
    meck:expect(user_management, is_valid, 2, User),
    %% Login
    {LogResult, _SessionId} = controller:handle_action(
                               {login, {ok, User}},
                               {Callback, []}),
    meck:unload(user_management),
    ?assertEqual({login, success}, LogResult).

%%-------------------------------------------------------------------
%% @doc
%% Sets up the tests
%% @end
%%-------------------------------------------------------------------
setup() ->
    error_logger:tty(false),
    ?debugVal("Starting ALL USER tests"),
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
    ?debugVal("Completing ALL USER tests").


%%-------------------------------------------------------------------
%% @doc
%% Sets up meck
%% @end
%%-------------------------------------------------------------------
setup_meck() ->
    meck:new(controller_app_worker_sup, [passthrough]),
    meck:new(controller_app_worker, [passthrough]),
    meck:new(user_management),
    meck:expect(controller_app_worker_sup, init,
                fun(_No_Arg) ->
                        initReturn(controller_app_worker) end).

%%-------------------------------------------------------------------
%% @doc
%% Unloads the controller_app_worker_sup
%% @end
%%-------------------------------------------------------------------
teardown_meck() ->
    meck:unload(controller_app_worker_sup),
    meck:unload(controller_app_worker),
    meck:unload(user_management).


%%-------------------------------------------------------------------
%% @doc
%% Creates a Supervisor and Childspec list for the mocked init function
%% @end
%%-------------------------------------------------------------------
initReturn(Module) ->
    {ok, {
       {one_for_one, 5, 10},
       [
        {1, {Module, start_link, []}, permanent, 5000, worker, [Module]},
        {2, {Module, start_link, []}, permanent, 5000, worker, [Module]},
        {3, {Module, start_link, []}, permanent, 5000, worker, [Module]},
        {4, {Module, start_link, []}, permanent, 5000, worker, [Module]},
        {5, {Module, start_link, []}, permanent, 5000, worker, [Module]},
        {6, {Module, start_link, []}, permanent, 5000, worker, [Module]}
       ]}
    }.


%% Test data
get_test_data(create_user) ->
    {1234, "user"};
get_test_data(get_user) ->
    {id, 1234, #user{}};
get_test_data(update_user) ->
    {#user{}};
get_test_data(login) ->
    {#user{id=5527647785738502144}, 5527647785738502144}.

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


update_rec_by_proplist_test_() ->
    [
       ?_assertEqual(
          #user{nick="lin", name="agner"},
          controller:update_rec_by_proplist(
              #user{nick="lin"},
              [{#user.password, field_missing}, {#user.name, "agner"}]
       )
     )].
