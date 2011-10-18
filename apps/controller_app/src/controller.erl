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

-export([create_user/2,
         login_user/1,
         new_game/1]).

-define(WORKER, controller_app_worker).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% API for creation of a user.
%% @end
%% [@spec create_user(Id::Integer(), #user{}) -> ok.
%% @end]
%%-------------------------------------------------------------------
%-spec create_user(Integer(), #user{}) -> ok.
create_user(Id, User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {create_user, Id, User}).

%%-------------------------------------------------------------------
%% @doc
%% API for logging in a user.
%% @end
%% [@spec login_user(#user{}) -> ok.
%% @end]
%%-------------------------------------------------------------------
%-spec login_user(#user{}) -> ok.
login_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {login_user, User}).


%%-------------------------------------------------------------------
%% @doc
%% API for creation of a game
%% @end
%% [@spec create_game(#game{}) -> ok.
%% @end]
%%-------------------------------------------------------------------
new_game(Game) ->
    gen_server:call(service_worker:select_pid(?WORKER), {new_game, Game}).
