%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for the session id interface.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_id_test).

-define(TIMEOUT, 3000).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").


%% startup
apps() ->
    [protobuffs, riakc, db].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

%% teardown
app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).


%% testing the session interface
session_id_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [
      ?_test(id_pid_conversion()),
      ?_test(illegal_id())
     ]
    }.


id_pid_conversion() ->
    OwnPid = self(),

    SessId = session_id:from_pid(OwnPid),
    SessPid = session_id:to_pid(SessId),

    ?assertEqual(OwnPid, SessPid).

illegal_id() ->
    Id = "123456789",
    Result = session_id:to_pid(Id),
    ?assertEqual({error, invalid_id}, Result).
