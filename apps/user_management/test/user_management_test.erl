-module(user_management_test).

-include_lib("eunit/include/eunit.hrl").

app_start() ->
    application:start(user_management).

app_stop(Client) ->
    db_c:disconnect(Client).

create_user(ok) ->
    Id = undefined,
    User = bob,
    ?assert(User == user_management:create(Id, User)).

create_user_with_key(ok) ->
    Id = 1,
    User = bob,
    ?assert(User == user_management:create(Id, User)).

user_management_create_test() ->
    {"create a user",
      {setup,
       fun app_start/0,
       fun app_stop/1,
       [fun create_user/1, fun create_user_with_key/1]}}.
