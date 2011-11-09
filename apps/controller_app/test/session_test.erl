%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Unit tests for the session interface.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_test).

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
session_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [
      ?_test(alive_t()),
      ?_test(start_stop_t())
     ]
    }.

alive_t() ->
    OwnId = session_id:from_pid(self()),
    FakeId = session_id:from_pid(list_to_pid("<0.9999.0>")),

    OwnAlive = session:alive(OwnId),
    ?assertEqual(true, OwnAlive),

    FakeAlive = session:alive(FakeId),
    ?assertEqual(false, FakeAlive).

start_stop_t() ->
    User = create_user(),
    Id = User#user.id,
    SessionId = session:start(User, session_history:create(Id)),

    Alive = session:alive(SessionId),
    ?assertEqual(true, Alive),

    session:stop(SessionId),
    % wait for it to stop
    MonitorRef = monitor(process, session_id:to_pid(SessionId)),
    receive {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok end,
    StopAlive = session:alive(SessionId),
    ?assertEqual(false, StopAlive).


%% helper functions
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


update_rec_by_proplist_test_() ->
    [
       ?_assertEqual(
          #user{nick="lin", name="agner"},
          session_proc:update_rec_by_proplist(
              #user{nick="lin"},
              [{#user.password, field_missing}, {#user.name, "agner"}]
       )
     )].

