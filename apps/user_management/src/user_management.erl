%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
         update/1,
         is_valid/2
        ]).


%%-------------------------------------------------------------------
%% @doc
%% Creates a new user and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
create(#user{id = IdIn} = UserIn) ->
    Id = case IdIn of
             undefined ->
                 db:get_unique_id();
             IdIn ->
                 IdIn
         end,
    User = UserIn#user{id = Id},

% @todo check if user with nick already exists
    BinId = db:int_to_bin(Id),
    DBVal = db_obj:create(?B_USER, BinId, User),
    db:put(DBVal),
    {ok, ReadItem} = db:get(?B_USER, BinId),
    db_obj:get_value(ReadItem).

%%-------------------------------------------------------------------
%% @doc
%% Updates an existing user and returns the result to the Client.
%%
%% @spec update(NewUser::#user{}) -> ok | {error, any()}
%% @end
%%-------------------------------------------------------------------
update(#user{id = Id} = NewUser) when is_integer(Id) ->
    case db:get(?B_USER, db:int_to_bin(Id)) of
        {ok, Obj} ->
            NewObj = db_obj:set_value(Obj, NewUser),
            db:put(NewObj),
            {ok, NewUser};
        Error ->
            {error, Error}
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
