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
%%% @author Stephan Brandauer <stephan.brandauer@gmail.com>
%%%
%%% @doc common module for replication of mnesia tables during startup of
%%% backends
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mnesia_utils).

-export([replicate/1]).

-include_lib("datatypes/include/push_event.hrl").
-include_lib("utils/include/debug.hrl").

%%-------------------------------------------------------------------
%% @doc
%% replicates a given table from one of the backends in
%% `application:get_env(backend_nodes)'.
%% If no reachable backend is found
%% (perhaps because this backend is the first!), `{error, no_reachable_backend}'
%% is returned.
%% @end
%%-------------------------------------------------------------------
-spec replicate (Table :: atom ()) ->
                        ok | {error, no_reachable_backend}.
replicate (Table) ->
    Backend = get_responding_backend (),
    case Backend of
        undefined ->
            {error, no_reachable_backend};
        Backend ->
            ?DEBUG ("copying table '~p' from ~p~n",
                    [Table, Backend]),
            This = node (),
            {ok, _} =
                rpc:call (Backend,
                          mnesia,
                          change_config,
                          [extra_db_nodes, [This]]),
            mnesia:add_table_copy (Table, node (), ram_copies),
            ?DEBUG ("copying done~n"),
            ok
    end.

%%-------------------------------------------------------------------
%% @doc
%% Go through the backend_nodes-list in the app file and return the first one
%% that is not equal to `node()' AND that is ping-able.
%% @todo this now reads from the controller_app config because no decision was mode made where this should go
%% @end
%%-------------------------------------------------------------------
-spec get_responding_backend() -> Node | undefined when
      Node :: atom().
get_responding_backend () ->
    case application:get_env (controller_app, backend_nodes) of
        undefined -> undefined;
        {ok, Backends} -> get_responding_backend (Backends)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Go through a list of nodes and return the first one that is not equal to
%% `node()' AND that is ping-able.
%% @end
%%-------------------------------------------------------------------
-spec get_responding_backend([Node]) -> Node | undefined when
      Node :: atom().
get_responding_backend ([]) ->
    undefined;
get_responding_backend ([Backend | Backends]) ->
    case node () of
        Backend ->
            get_responding_backend (Backends);
        _ ->
            case net_adm:ping (Backend) of
                pang ->
                    get_responding_backend (Backends);
                pong ->
                    Backend
            end
    end.
