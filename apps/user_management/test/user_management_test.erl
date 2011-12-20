%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
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

assign_mod_test_() ->
    {"add moderator test",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [add_moderator_t(),
      remove_moderator_t()]
     }}.

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

create_user_with_taken_nick_test_() ->
    {"create a user with a nick that is already taken",
      {setup,
       fun () ->
               app_start(),
               (create_user())#user{id=undefined}
       end,
       fun app_stop/1,
       fun create_user_nick_taken_t/1
      }}.

update_user_test_() ->
    {"update a user",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      update_user()
      }}.

update_non_existing_user_test_() ->
    {"update a non exisiting user",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      update_non_existing_user()
      }}.

get_user_test_() ->
    {"create and re-read a user",
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [get_user_t(),
       get_user_fail_t(),
       get_dbobj_user_t()]
     }}.

get_user_by_search_test_() ->
    {"get a user by search",
     {setup,
      fun() ->
              ?debugMsg("set up get by search"),
              app_start(),
              Result = user_management:create(create_user()),
              {ok, User} = Result,
              [
               {User, #user.nick},
               {User, #user.role},
               {User, #user.score},
               {User, #user.email}
              ]
      end,
      fun app_stop/1,
      fun get_user_by_search_instantiator/1
      }}.

get_user_id_by_search_test_() ->
    {"get a user id by search",
     {setup,
      fun() ->
              ?debugMsg("set up get id by search"),
              app_start(),
              Result = user_management:create(create_user()),
              {ok, User} = Result,
              [
               {User, #user.nick},
               {User, #user.role},
               {User, #user.score},
               {User, #user.email}
              ]
      end,
      fun app_stop/1,
      fun get_user_id_by_search_instantiator/1
      }}.

get_user_t() ->
    fun() ->
            {ok, User} = user_management:create(create_user()),
            Result = user_management:get(User#user.id),
            ?assertEqual(User, Result)
    end.

get_user_fail_t() ->
    fun() ->
            Result = user_management:get(db_c:get_unique_id()),
            ?assertEqual({error, notfound}, Result)
    end.

get_dbobj_user_t() ->
    [{"Test get db object of user",
     fun() ->
            {ok, User} = user_management:create(create_user()),
            {ok, DbObj} = user_management:get_db_obj(User#user.id),
            UserPropList = db_obj:get_value(DbObj),
            ResultUser = data_format:plist_to_rec(?USER_REC_NAME, UserPropList),
            ?assertEqual(User, ResultUser)
    end},
     {"Test get db object of user when user not exist",
     fun() ->
            Result= user_management:get_db_obj(db_c:get_unique_id()),
            ?assertEqual({error, notfound}, Result)
    end}].

blacklist_test_() ->
    {"Blacklist a user",
     {setup,
      fun() ->
              app_start(),
              Result = user_management:create(create_user()),
              {ok, User} = Result,
              User
      end,
      fun app_stop/1,
      fun blacklist_instantiator/1
      }}.

whitelist_test_() ->
    {"Whitelist a user",
     {setup,
      fun() ->
              app_start(),
              Result = user_management:create(create_user()),
              {ok, User} = Result,
              User
      end,
      fun app_stop/1,
      fun whitelist_instantiator/1
      }}.

%% tests generators
create_user_t(#user{} = User) ->
    Result = user_management:create(User),
    ?debugVal(User),
    ?debugVal(Result),
    {ok, ResUser} = Result,
    CreateDate = ResUser#user.date_created,
    UserRec = User#user{id = ResUser#user.id, date_created = CreateDate},
    ?assertEqual(UserRec, ResUser),
    GetRes = user_management:get(ResUser#user.id),
    ?_assertEqual(ResUser, GetRes).

create_user_nick_taken_t(#user{} = User) ->
    {ok, User1} = user_management:create(User),
    ?debugVal(User1),
    Result = user_management:create(User),
    ?_assertEqual({error, nick_already_exists}, Result).

update_user() ->
    fun() ->
            {ok, User} = user_management:create(create_user()),
            UpdatedUser = User#user{name = "Updated Name"},
            Result = user_management:update(UpdatedUser),
            ?assertEqual({ok, UpdatedUser}, Result)
    end.

update_non_existing_user() ->
    fun() ->
            User = create_user(),
            Result = user_management:update(User),
            ?assertEqual({error, does_not_exist}, Result)
    end.

get_user_by_search_instantiator(List) ->
    lists:map(fun({#user{} = User, Field}) ->
                      ?_test(get_user_by_search(User, Field))
              end, List).

get_user_by_search(#user{} = User, Field) ->
    Value = element(Field, User),
    Result = user_management:get(Field, Value),
    case Result of
        {ok, {index_list, IdxList}} ->
            ?assert(is_list(IdxList));
        {ok, ResultUser} ->
            ?assertEqual(User, ResultUser);
        Other ->
            erlang:error(error, Other)
    end.

get_user_id_by_search_instantiator(List) ->
    lists:map(fun({#user{} = User, Field}) ->
                      ?_test(get_user_id_by_search(User, Field))
              end, List).

get_user_id_by_search(#user{} = User, Field) ->
    Value = element(Field, User),
    Result = user_management:get_id(Field, Value),
    case Result of
        {ok, {index_list, IdxList}} ->
            ?assert(is_list(IdxList));
        {ok, ResultUserId} ->
            ?assertEqual(User#user.id, ResultUserId);
        Other ->
            erlang:error(error, Other)
    end.

add_moderator_t() ->
    fun() ->
            {ok, User} = user_management:create(create_user()),
            Username = User#user.nick,
            ExpectedUser = User#user{role = moderator},
            {ok, Result} = user_management:assign_moderator(Username, add),
            ?assertEqual(ExpectedUser, Result)
    end.

remove_moderator_t() ->
    fun() ->
            {ok, User} = user_management:create(create_user()),
            Username = User#user.nick,
            ExpectedUser = User#user{role = user},
            user_management:assign_moderator(Username, add),
            {ok, Result} = user_management:assign_moderator(Username, remove),
            ?assertEqual(ExpectedUser, Result)
    end.

blacklist_instantiator(#user{} = User) ->
    fun() ->
            user_management:blacklist(User#user.nick),
            {ok, BlacklistUser} = user_management:get(#user.id, User#user.id),
            ?assertEqual(disabled, BlacklistUser#user.role)
    end.

whitelist_instantiator(#user{} = User) ->
    fun() ->
            % We first blacklist and user and then test if whitelist works
            user_management:blacklist(User#user.nick),
            {ok, BlacklistUser} = user_management:get(#user.id, User#user.id),
            ?assertEqual(disabled, BlacklistUser#user.role),
            user_management:whitelist(User#user.nick),
            {ok, Whitelist} = user_management:get(#user.id, User#user.id),
            ?assertEqual(user, Whitelist#user.role)
    end.

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
