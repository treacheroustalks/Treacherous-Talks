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

%% Public application interface
-export([
         create/1,
         get/1, get/2,
         get_by_idx/2,
         update/1,
         is_valid/2
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
    case get_by_idx(#user.nick, UserIn#user.nick) of
        {ok, _CurUser} ->
            {error, nick_already_exists};
        {error, does_not_exist} ->
            Id = case IdIn of
                     undefined ->
                         db:get_unique_id();
                     IdIn ->
                         IdIn
                 end,
            User = UserIn#user{id = Id},

            BinId = db:int_to_bin(Id),
            DbObj = db_obj:create(?B_USER, BinId, User),
            DbObj2 = db_obj:set_indices(DbObj, create_idx_list(User)),
            db:put(DbObj2),
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
    case db:get(?B_USER, db:int_to_bin(Id)) of
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
            NewObj = db_obj:set_value(Obj2, NewUser),
            NewObj2 = db_obj:set_indices(NewObj, create_idx_list(NewUser)),
            db:put(NewObj2),
            {ok, NewUser};
        {error, notfound} ->
            {error, does_not_exist};
        Error ->
            {error, Error}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Queries a user by index.
%% @end
%%-------------------------------------------------------------------
get_by_idx(Field, Val) ->
    case create_idx(Field, Val) of
        {error, field_not_indexed} ->
            {error, field_not_indexed};
        Idx ->
            case db:get_index(?B_USER, Idx) of
                {ok, [[?B_USER, Key]]} ->
                    {ok, DbObj} = db:get(?B_USER, Key),
                    {ok, DbObj};
                {ok, []} ->
                    {error, does_not_exist};
                {ok, List} ->
                    {ok, {index_list, List}};
                Other ->
                    {error, Other}
            end
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets user from the database.
%% @end
%%-------------------------------------------------------------------
get(Id) ->
    BinId = db:int_to_bin(Id),
    case db:get(?B_USER, BinId) of
        {ok, RiakObj} ->
            db_obj:get_value(RiakObj);
        {error, Error} ->
            {error, Error};
        Other ->
            erlang:error({error, {unhandled_case, Other, {?MODULE, ?LINE}}})
    end.

get(Type, Key) ->
    {ok, Keys} = db:list_keys(?B_USER),
    lists:foldl(
      fun(Id, Acc) ->
              {ok, Item} = db:get(?B_USER, Id),
              Value = db_obj:get_value (Item),
              if
                  Key == '_' ->
                      [db_obj:get_value(Item) | Acc];
                  element (Type, Value) == Key ->
                      [db_obj:get_value(Item) | Acc];
                  true -> Acc
              end
      end,
      [], Keys).

%%-------------------------------------------------------------------
%% @doc
%% Verifies user authentication and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
is_valid(Nick, Password) ->
    % @todo use secondary indices for this!!!

    % This is terrible, but map/reduce erlang code would need to be on the
    % riak node. for now this is the solution...
    % http://lists.basho.com/pipermail/riak-users_lists.basho.com/2010-April/000988.html
    % https://gist.github.com/351659
    % or add a load path to the riak node, where the code is:
    % http://markmail.org/message/xvttgfnufojqbv7w#query:+page:1+mid:xvttgfnufojqbv7w+state:results
    % or use javascript functions ...

    {ok, Keys} = db:list_keys(?B_USER),
    Result = lists:foldl(
               fun(Key, Acc) ->
                       {ok, Item} = db:get(?B_USER, Key),
                       Val = db_obj:get_value(Item),
                       case Val of
                           #user{nick = Nick, password = Password} ->
                               [Val | Acc];
                           _ ->
                               Acc
                       end
               end, [], Keys),
    case Result of
        [] ->
            false;
        [User | _] ->
            User
    end.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Creates the index list for the database
%% @end
%%-------------------------------------------------------------------
create_idx_list(#user{nick=Nick, role=Role, score=Score, email=Mail}) ->
    [
     create_idx(#user.nick, Nick),
     create_idx(#user.role, Role),
     create_idx(#user.score, Score),
     create_idx(#user.email, Mail)
    ].
%%-------------------------------------------------------------------
%% @doc
%% Creates an index tuple for the database.
%% @end
%%-------------------------------------------------------------------
create_idx(#user.nick, Nick) ->
    {<<"nick_bin">>, list_to_binary(Nick)};
create_idx(#user.role, Role) ->
    {<<"role_bin">>, term_to_binary(Role)};
create_idx(#user.score, Score) ->
    {<<"score_int">>, Score};
create_idx(#user.email, Mail) ->
    {<<"email_bin">>, list_to_binary(Mail)};
create_idx(_, _) ->
    {error, field_not_indexed}.
