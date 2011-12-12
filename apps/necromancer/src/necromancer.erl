%% -----------------------------------------------------------------------------
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

%% @doc
%%  Makes it possible to 'watch' a erlang VM by name.
%%  A watched VM has the possibility to store data about it's processes
%%  persistently (in a place where other VMs have access) as long as
%%  everything is fine. Every stored blob of data is called a 'corpse'.<br/>
%%  A corpse (see the type definition below!) consists of the owner-node, the
%%  module that has it's `handle_corpse' function and of the data, that function
%%  needs in order to rebuild it.<br/>
%%  If the watched VM dies, the watching VM will call the supplied `get_corpses'
%%  function which gives it a list of corpses that where written by that node.
%%  The necromancer module will take care of giving those corpses to the other,
%%  still living nodes, whose `handle_corpse' functions are responsible for
%%  cleaning the mess up or resurrecting the processes.
%%  They also have to <em>remove</em> the corpse after cleanup. <br/>
%%  This module does not know anything about how you store your data - and it
%%  doesn't want to! All it needs is your implementation of a `get_corpses'
%%  function and that you save all the information that you need to
%%  clean up again.
%% @TODO make sure that no nodes are added more often than once.
%% @TODO when a node starts, make it watch exactly one other node via {@link watch/1}
%% @end
%% -----------------------------------------------------------------------------
-module(necromancer).
-vsn("1.0.0").

-include_lib ("utils/include/debug.hrl").

-export ([start_link/1,
          watch/1,
          unwatch/1,
          get_watched/0]).

-type corpse () :: {node (), module (), Data :: any ()}.
-type get_corpses_fun () :: fun ((Node :: node ()) -> [corpse ()] | []).

-export_type ([corpse/0]).

-record (state,
         {nodes = [] :: [node ()],
          get_corpses_fun = erlang:error ({error,
                                           {?MODULE,
                                            get_corpses_fun,
                                            required}}) ::
                              get_corpses_fun ()
        }).

-spec start_link (GetCorpsesFun) -> {ok, pid ()} when
      GetCorpsesFun :: get_corpses_fun ().
start_link (GetCorpsesFun) ->
    Pid = spawn_link (fun () ->
                              loop (#state{get_corpses_fun = GetCorpsesFun,
                                           nodes = []})
                      end),
    case catch register (necromancer_srv, Pid) of
        true ->
            ok;
        Else ->
            ?DEBUG ("COULD NOT REGISTER BECAUSE OF ~p~n", [Else]),
            erlang:error ({error, {?MODULE, could_not_start, Else}})
    end,
    {ok, Pid}.

%% -----------------------------------------------------------------------------
%% @doc
%% adds one or several nodes to the list of watched nodes.
%% If the node is not running yet, a process will be spawned that will wait for
%% the node to appear and then add it. It's fire-and-forget.
%% @end
%% -----------------------------------------------------------------------------
-spec watch ([node ()] | node ()) -> ok.
watch (N) ->
    necromancer_srv ! {watch, N},
    ok.

-spec get_watched () -> [node ()] | [].
get_watched () ->
    necromancer_srv ! {get_watched, self ()},
    receive
        {watched, Nodes} ->
            Nodes
    after
        10000 ->
            []
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% removes one or several nodes from the list of watched nodes.
%% @end
%% -----------------------------------------------------------------------------
-spec unwatch ([node ()] | node ()) -> ok.
unwatch (N) ->
    necromancer_srv ! {unwatch, N},
    ok.

-spec loop (#state{}) -> no_return ().
loop (State = #state{nodes = Nodes}) ->
    receive
        {nodedown, DeadNode} ->
            handle_death (DeadNode, State),
            loop (State#state{nodes = lists:delete (DeadNode, Nodes)});
        {watch, NewNodes} when is_list (NewNodes) ->
            lists:foreach (fun (N) -> self () ! {watch, N} end, NewNodes),
            loop (State);
        {watch, NewNode} ->
            NewState = do_watch (NewNode, State),
            loop (NewState);
        {unwatch, Nodes} when is_list (Nodes) ->
            lists:foreach (fun (N) -> self () ! {unwatch, N} end, Nodes),
            loop (State);
        {unwatch, Node} ->
            NewState = do_unwatch (Node, State),
            loop (NewState);
        {get_watched, From} ->
            From ! {watched, State#state.nodes},
            loop (State)
    end.

-spec do_watch (node (), #state{}) -> #state{}.
do_watch (Node, State = #state{nodes = Nodes}) ->
    ?DEBUG ("watch(~p)~n", [Node]),
    case net_adm:ping (Node) of
        pong ->
            erlang:monitor_node (Node, true),
            State#state{nodes = [Node | Nodes]};
        pang ->
            ?DEBUG ("~p is not reachable.~n", [Node]),
            spawn (fun () -> wait_for_node (Node) end),
            State
    end.

wait_for_node (Node) ->
    timer:sleep (1000),
    case net_adm:ping (Node) of
        pong ->
            ?DEBUG ("~p is finally reachable!~n", [Node]),
            necromancer_srv ! {watch, Node};
        pang ->
            wait_for_node (Node)
    end.

-spec do_unwatch (node (), #state{}) -> #state{}.
do_unwatch (Node, State = #state{nodes = Nodes}) ->
    ?DEBUG ("unwatch(~p)~n", [Node]),
    erlang:monitor_node (Node, false),
    State#state{nodes = lists:delete (Node, Nodes)}.

-spec handle_death (node (), #state{}) -> ok.
handle_death (DeadNode, State = #state{get_corpses_fun = GetCorpses}) ->
    ?DEBUG ("Node ~p died. Handling death...~n", [DeadNode]),
    AliveNodes = [node () | lists:delete (DeadNode, State#state.nodes)],
    Corpses = GetCorpses (DeadNode),
    ?DEBUG ("Corpses: ~p~n", [Corpses]),
    ?DEBUG ("AliveNodes: ~p~n", [AliveNodes]),
    HandleCorpse = fun (Corpse, AliveNode) ->
                           ?DEBUG ("handling corpse ~p on node ~p~n",
                                   [Corpse, AliveNode]),
                           {Module, Data} = Corpse,
                           rpc:call (AliveNode,
                                     Module,
                                     handle_corpse,
                                     [Data])
                   end,
    lists:foreach (fun (Corpse) ->
                           Index = random:uniform (length (AliveNodes)),
                           RandomNode = lists:nth (Index, AliveNodes),
                           HandleCorpse (Corpse, RandomNode)
                   end,
                   Corpses),
    ok.
