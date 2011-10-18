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
-export([create/2, create/3]).


%%-------------------------------------------------------------------
%% @doc
%% Creates a new user and returns the result to the Client.
%% @end
%%-------------------------------------------------------------------
create(Id, #user{} = User) ->
    gen_server:call(service_worker:select_pid(user_management_worker),
                    {create_user, Id, User}).
create(Client, Id, #user{} = User) ->
    gen_server:cast(service_worker:select_pid(user_management_worker),
                    {create_user, Client, Id, User}).
