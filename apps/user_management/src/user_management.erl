%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc The user management app provides all the CRU users functionality.
%%%
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(user_management).

-include_lib("datatypes/include/user.hrl").

%% Public application interface
-export([create/1, create/2, update/1, update/2, is_valid/2, is_valid/3]).


%%-------------------------------------------------------------------
%% @doc
%% Creates a new user and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
create(#user{id = Id} = User) ->
    gen_server:call(service_worker:select_pid(user_management_worker),
                    {create_user, Id, User}).
create(Client, #user{id = Id} = User) ->
    gen_server:cast(service_worker:select_pid(user_management_worker),
                    {create_user, Client, Id, User}).

%%-------------------------------------------------------------------
%% @doc
%% Updates an existing user and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
update(#user{id = Id} = NewUser) when is_integer(Id) ->
    gen_server:call(service_worker:select_pid(user_management_worker),
                    {create_user, Id, NewUser}).
update(Client, #user{id = Id} = User) when is_integer(Id) ->
    gen_server:cast(service_worker:select_pid(user_management_worker),
                    {create_user, Client, Id, User}).
    

%%-------------------------------------------------------------------
%% @doc
%% Verifies user authentication and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
is_valid(Nick, Password) ->
    gen_server:call(service_worker:select_pid(user_management_worker),
                    {is_valid, Nick, Password}).
is_valid(Client, Nick, Password) ->
    gen_server:cast(service_worker:select_pid(user_management_worker),
                    {is_valid, Client, Nick, Password}).
