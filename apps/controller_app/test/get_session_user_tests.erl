%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for getting the session user.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_session_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-export([success/2, invalid/2]).
%%-------------------------------------------------------------------
%% Update user tests
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    Data = get_test_data(success),
    Cmd = {get_session_user, {ok, SessId, Data}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, User} = Result,

    ?assertEqual({get_session_user, success}, CmdRes),
    ?assert(is_record(User, user)).

invalid(_Callback, _SessId) ->
    % no failing case if session valid
    ok.
%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
get_test_data(success) ->
    no_arg;
get_test_data(invalid) ->
    ok.
