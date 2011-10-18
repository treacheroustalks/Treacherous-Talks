-module(controller_app_test).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------
%% @doc
%% Tests the create functionality in the controller
%% The user_management_app_worker is a mocked module
%% @end
%%-------------------------------------------------------------------
controller_create_test() ->
    {Key, User} = {1234, "user"},
    setup(),
    meck:new(user_management),
    meck:expect(user_management, create,
                fun(_From, _Key, _User) -> ok end),
    meck:new(controller_app_worker, [passthrough]),
    meck:expect(controller_app_worker, handle_call,
                fun({create, Id, Usr}, From, State) ->
                        user_management:create(From, Id, Usr),
                        {reply, From, State} end),
    {From, _Ref} = controller:create(Key, User),
    ?assertEqual(self(), From),
    teardown(),
    meck:unload(user_management).


%%-------------------------------------------------------------------
%% @doc
%% Sets up the test, with a mocked init function in the
%% controller_app_worker_sup
%% @end
%%-------------------------------------------------------------------
setup() ->
    meck:new(controller_app_worker_sup, [passthrough]),
    meck:expect(controller_app_worker_sup, init,
                fun(_No_Arg) ->
                        initReturn(controller_app_worker) end),
    controller_app_sup:start_link().
%%-------------------------------------------------------------------
%% @doc
%% Unloads the controller_app_worker_sup
%% @end
%%-------------------------------------------------------------------
teardown() ->
    meck:unload(controller_app_worker_sup).

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
