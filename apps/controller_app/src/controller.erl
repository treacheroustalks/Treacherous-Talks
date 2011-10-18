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
-export([create/2]).
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
