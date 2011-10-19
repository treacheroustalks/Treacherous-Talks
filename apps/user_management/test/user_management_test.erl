-module(user_management_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").


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
               {undefined, create_user()}
       end,
       fun app_stop/1,
       fun create_user_t/1
      }}.

create_user_with_key_test_() ->
    {"create a user with key",
      {setup,
       fun () ->
               app_start(),
               {12345, create_user()}
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
               User1 = user_management:create(undefined, User),
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
               user_management:create(undefined, User),
               {User, "S0m3_rand0m_passw0rd", false}
       end,
       fun app_stop/1,
       fun authenticate_user/1
      }}.


%% tests generators
create_user_t({Id, #user{} = User}) ->
    Result = user_management:create(Id, User),
    ?debugVal(User),
    ?debugVal(Result),
    ?_assert(User#user{id = Result#user.id} == Result).

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
