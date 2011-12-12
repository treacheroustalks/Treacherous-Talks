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
-module(controller_app_app).
-vsn("1.0.0").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, handle_corpse/1]).

-include_lib("utils/include/debug.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?DEBUG("[~p] starting~n", [?MODULE]),
    necromancer_app:start (normal, []),
    case backends:watch_neighbour () of
        undefined ->
            ?DEBUG ("Didn't find left neighbour. Shouldn't I watch someone?~n");
        _Node ->
            ?DEBUG ("watching ~p~n", [_Node])
    end,
    corpses:save_corpse(?MODULE, node(), {node, node()}),
    ok = session_presence:init(),
    controller_app_sup:start_link().

stop(_State) ->
    ok.

handle_corpse ({Key, {node, Node}}) ->
    NewBackendNodes = lists:delete (Node, backends:get_all_backends ()),
    NotifyOfDeath = fun (RemoteBackend) ->
                            rpc:call (RemoteBackend,
                                      backends,
                                      change_state,
                                      [backend_nodes, NewBackendNodes])
                    end,
    lists:foreach (NotifyOfDeath, NewBackendNodes),
    db:delete (Key).

