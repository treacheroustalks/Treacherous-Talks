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

%%-------------------------------------------------------------------
%% @doc
%% Runs all tests
%% @end
%%-------------------------------------------------------------------
controller_test_() ->
    [
     fun setup/0,
     fun controller_create_user/0,
     fun controller_login_user/0,
     fun controller_update_user/0,
     fun controller_get_user/0,
     fun controller_new_game/0,
     fun teardown/0
    ].


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
    {Key, User} = get_test_data(create_user),
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


%%-------------------------------------------------------------------
%% @doc
%% Tests the get_user functionality in the controller
%% @end
%%-------------------------------------------------------------------
controller_get_user() ->
    ?debugVal("Testing geting a user"),
    setup_meck(),
    {Type, Key, User} = get_test_data(get_user),
    meck:expect(user_management, get,
                fun(_From, _Type, _Key) -> ok end),
    meck:expect(controller_app_worker, handle_call,
                fun({get_user, Type1, Key1}, From, State) ->
                        user_management:get(From, Type1, Key1),
                        {reply, From, State} end),
    {From, _Ref} = controller:get_user(Type, Key),
    ?assertEqual(self(), From),
    teardown_meck(),
    ?debugVal("Completed getting user test").


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
%% Sets up the tests
%% @end
%%-------------------------------------------------------------------
setup() ->
    error_logger:tty(false),
    ?debugVal("Starting ALL tests"),
    % Application has to be loaded to get env variables
    application:load(controller_app),
    ?debugVal(controller_app_sup:start_link()).


%%-------------------------------------------------------------------
%% @doc
%% Cleanup tests
%% @end
%%-------------------------------------------------------------------
teardown() ->
    ?debugVal("Completing ALL tests").


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


fake_game_setup() ->
    meck:new(game),
    meck:expect(game, new_game,
                fun(From, _Game) ->
                        gen_server:reply(From, {was_called, new_game})
                end).


fake_game_teardown() ->
    meck:unload(game).


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