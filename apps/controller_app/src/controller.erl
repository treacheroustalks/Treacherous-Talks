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
-export([create/2, new_game/1]).
-define(WORKER, controller_app_worker).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% API for creation of a user.
%% @end
%% [@spec create(#request{}, Id::Integer(), #user{}) -> ok.
%% @end]
%%-------------------------------------------------------------------
%-spec create(#request{}, Integer(), #user{}) -> ok.
create(Id, User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {create, Id, User}).


%%-------------------------------------------------------------------
%% @doc
%% API for creation of a game
%% @end
%% [@spec create_game(#game{}) -> ok.
%% @end]
%%-------------------------------------------------------------------
new_game(Game) ->
    gen_server:call(service_worker:select_pid(?WORKER), {new_game, Game}).
