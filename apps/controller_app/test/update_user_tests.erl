%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for updating user
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(update_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([success/2, invalid/2]).
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    Data = get_test_data(success),
    Cmd = {update_user, {ok, SessId, Data}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, _Info} = Result,

    ?assertEqual({update_user, success}, CmdRes).

invalid(Callback, SessId) ->
    % don't know how this could fail ...
    ok.
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    [{#user.password,"QWER"},
     {#user.email,field_missing},
     {#user.name,"Agner Erlang"}];
get_test_data(invalid) ->
    ok.
