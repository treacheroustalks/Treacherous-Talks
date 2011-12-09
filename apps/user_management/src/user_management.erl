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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc The user management lib provides all the CRU users functionality.
%%%
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(user_management).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib ("eunit/include/eunit.hrl").

%% Public application interface
-export([
         assign_moderator/2,
         create/1,
         get/1,
         get_id/2,
         get_db_obj/1,
         get/2,
         update/1
        ]).


%%-------------------------------------------------------------------
%% @doc
%% Creates a new user and returns the result to the Client.
%%
%% @spec create(#user{}) ->
%%         {ok, #user{}} |
%%         {error, nick_already_exists} |
%%         {error, any()}
%% @end
%%-------------------------------------------------------------------
create(#user{id = IdIn} = UserIn) ->
    case get(#user.nick, UserIn#user.nick) of
        {ok, _CurUser} ->
            {error, nick_already_exists};
        {error, does_not_exist} ->
            Id = case IdIn of
                     undefined ->
                         db:get_unique_id();
                     IdIn ->
                         IdIn
                 end,
            User = UserIn#user{id = Id, date_created = erlang:universaltime()},

            BinId = db:int_to_bin(Id),
            %store as proplist for search
            UserPropList = data_format:rec_to_plist(User),
            DbObj = db_obj:create(?B_USER, BinId, UserPropList),
            db:put(DbObj, [{w, 1}]),
            {ok, User};
        Other ->
            {error, Other}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Updates an existing user and returns the result to the Client.
%%
%% @spec update(NewUser::#user{}) ->
%%         {ok, #user{}} | {error, doesn_not_exist} | {error, any()}
%% @end
%%-------------------------------------------------------------------
update(#user{id = Id} = NewUser) when is_integer(Id) ->
    BinId = db:int_to_bin(Id),
    case db:get(?B_USER, BinId, [{r,1}]) of
        {ok, Obj} ->
            Obj2 = case db_obj:has_siblings(Obj) of
                       false ->
                           Obj;
                       true ->
                           % If we get siblings at this stage (after login),
                           % we have an old session writing => overwrite it
                           [H|_] = db_obj:get_siblings(Obj),
                           H
                   end,
            NewUserPropList = data_format:rec_to_plist(NewUser),
            NewObj = db_obj:set_value(Obj2, NewUserPropList),
            db:put(NewObj, [{w, 1}]),
            {ok, NewUser};
        {error, notfound} ->
            {error, does_not_exist};
        Error ->
            {error, Error}
    end;
update(_User) ->
    {error, does_not_exist}.

%%-------------------------------------------------------------------
%% @doc
%%  Search for a user if success return user record
%% @end
%%-------------------------------------------------------------------
-spec get(integer(), any()) ->
          {ok, #user{}} |
          {ok, {index_list, [#user{}]}} |
          {error, does_not_exist} |
          {error, term()}.
get(Field, Val) ->
    {ok, Query} = db_utils:get_search_term(Field, Val, ?USER_REC_NAME),

    case db_utils:do_search_values(?B_USER, Query, ?USER_REC_NAME) of
        {ok, [User = #user{}]} ->
            {ok, User};
        {ok, []} ->
            {error, does_not_exist};
        {ok, List} ->
            {ok, {index_list, List}};
        Other ->
            {error, Other}
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Search for a user if success return user id
%% @end
%%-------------------------------------------------------------------
-spec get_id(integer(), any()) ->
          {ok, integer()} |
          {ok, {index_list, [integer()]}} |
          {error, does_not_exist} |
          {error, term()}.
get_id(Field, Val) ->
    {ok, Query} = db_utils:get_search_term(Field, Val, ?USER_REC_NAME),

    case db_utils:do_search(?B_USER, Query) of
        {ok, [Id]} ->
            {ok, Id};
        {ok, []} ->
            {error, does_not_exist};
        {ok, List} ->
            {ok, {index_list, List}};
        Other ->
            {error, Other}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets user from the database.
%% @end
%%-------------------------------------------------------------------
-spec get(integer()) ->
          #user{} | {error, any()}.
get(Id) ->
    BinId = db:int_to_bin(Id),
    case db:get(?B_USER, BinId, [{r,1}]) of
        {ok, RiakObj} ->
            UserPropList = db_obj:get_value(RiakObj),
            data_format:plist_to_rec(?USER_REC_NAME, UserPropList);
        {error, Error} ->
            {error, Error};
        Other ->
            erlang:error({error, {unhandled_case, Other, {?MODULE, ?LINE}}})
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets user from the database and returns DB object.
%%
%% @spec get_db_obj(integer()) ->
%%     {ok, #db_obj{}} | {error, any()}
%%
%% @end
%%-------------------------------------------------------------------
get_db_obj(Id) ->
    BinId = db:int_to_bin(Id),
    case db:get(?B_USER, BinId, [{r, all}]) of
        {ok, RiakObj} ->
            {ok, RiakObj};
        {error, Error} ->
            {error, Error};
        Other ->
            erlang:error({error, {unhandled_case, Other, {?MODULE, ?LINE}}})
    end.

%%-------------------------------------------------------------------
%% @doc
%% Updates an existing user, to add the moderator role or to remove
%% the moderator role.
%%
%% @spec assign_moderator(Username :: string(), Action :: atom()) ->
%%         {ok, #user{}} | {error, user_not_found}
%% @end
%%-------------------------------------------------------------------
assign_moderator(Username, Action) ->
    case get(#user.nick, Username) of
        {ok, {index_list, _UserList}} ->
            {error, user_not_found};
        {ok, User} ->
            case Action of
                add ->
                    ModUser = User#user{role = moderator};
                remove ->
                    ModUser = User#user{role = user}
            end,
            update(ModUser);
        _Error ->
            {error, user_not_found}
    end.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------
