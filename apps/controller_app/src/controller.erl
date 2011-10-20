%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Tiina Loukusa <loukusa@gmail.com>
%%%
%%% @doc Provides an API for the Treacherous Talks frontend
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller).

-export([create_user/1,
         get_user/1, get_user/2,
         update_user/1,
         login_user/1,
         new_game/1]).

-define(WORKER, controller_app_worker).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc create_user/2
%%
%% API for creation of a user
%% @end
%% [@spec create_user(Id::Integer(), #user{}) @end]
%%-------------------------------------------------------------------
create_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {create_user, User}).


%%-------------------------------------------------------------------
%% @doc update_user/1
%%
%% API for updating a user
%% @end
%% [@spec create_user(#user{}) @end]
%%-------------------------------------------------------------------
update_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {update_user, User}).


%%-------------------------------------------------------------------
%% @doc login_user/1
%%
%% API for logging in a user
%% @end
%% [@spec login_user(#user{}) @end]
%%-------------------------------------------------------------------
login_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {login_user, User}).


%%-------------------------------------------------------------------
%% @doc get_user/2
%%
%% API for getting a user
%% @end
%% [@spec get_user(integer()|string()) @end]
%%-------------------------------------------------------------------
get_user(Type, Key) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                   {get_user, Type, Key}).
get_user(Id) ->
    io:format ("get_user/2 :)~n"),
    gen_server:call(service_worker:select_pid(?WORKER),
                    {get_user, Id}).


%%-------------------------------------------------------------------
%% @doc
%% API for creation of a game
%% @end
%% [@spec create_game(#game{}) @end]
%%-------------------------------------------------------------------
new_game(Game) ->
    gen_server:call(service_worker:select_pid(?WORKER), {new_game, Game}).
