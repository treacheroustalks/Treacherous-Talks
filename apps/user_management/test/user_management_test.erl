-module(user_management_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").

-define(TIMEOUT, 10000).

%% startup
apps() ->
    [datatypes, service, protobuffs, riakc, db, user_management].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

%% teardown
app_stop(_Client) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%% main test
create_user_without_key_test_() ->
    {"create a user without key",
      {setup,
       fun () ->
               app_start(),
               (create_user())#user{id = undefined}
       end,
       fun app_stop/1,
       fun create_user_t/1
      }}.

create_user_with_key_test_() ->
    {"create a user with key",
      {setup,
       fun () ->
               app_start(),
               (create_user())#user{id=12345}
       end,
       fun app_stop/1,
       fun create_user_t/1
      }}.

authenticate_user_success_test_() ->
    {"authenticate a user",
      {setup,
       fun () ->
               app_start(),
               User = create_user(),
               User1 = user_management:create(User),
               {User1, User1#user.password, User1}
       end,
       fun app_stop/1,
       fun authenticate_user/1
      }}.

authenticate_user_failure_test_() ->
    {"authenticate a user",
      {setup,
       fun () ->
               app_start(),
               User = create_user(),
               user_management:create(User),
               {User, "S0m3_rand0m_passw0rd", false}
       end,
       fun app_stop/1,
       fun authenticate_user/1
      }}.

update_user_test_() ->
    {"update a user",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      fun update_user/0
      }}.

get_user_test_() ->
    {"create and re-read a user",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [fun get_user_t/0,
       fun get_user_fail_t/0,
       fun get_user_key_t/0]
      }}.

get_user_t() ->
    fun() ->
            User = user_management:create(create_user()),
            Result = user_management:get(User#user.id),
            ?assertEqual(User, Result)
    end.

get_user_key_t () ->
    fun () ->
            User = user_management:create(create_user ()),
            Result = user_management:get(#user.nick, User#user.nick),
            ?assert(is_list(Result))
    end.

get_user_fail_t() ->
    fun() ->
            Result = user_management:get(db_c:get_unique_id()),
            ?assertEqual({error, notfound}, Result)
    end.

%% tests generators
create_user_t(#user{} = User) ->
    Result = user_management:create(User),
    ?debugVal(User),
    ?debugVal(Result),
    ?_assert(User#user{id = Result#user.id} == Result).

update_user() ->
    fun() ->
            User = user_management:create(create_user()),
            UpdatedUser = User#user{name = "Updated Name"},
            Result = user_management:update(UpdatedUser),
            ?assertEqual({ok, UpdatedUser}, Result)
    end.

authenticate_user({#user{nick = Nick}, Pw, Expected}) ->
    ?debugVal(Expected),
    Result = user_management:is_valid(Nick, Pw),
    ?debugVal(Result),
    ?_assert(Expected == Result).


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
