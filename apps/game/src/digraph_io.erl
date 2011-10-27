%% -----------------------------------------------------------------------------
%% @doc
%% The module is responsible for serializing/deserializing a digraph
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (digraph_io).

-export ([to_erlang_term/1,
          from_erlang_term/1]).

-include_lib ("eunit/include/eunit.hrl").

-record (digraph, {vertices=[], edges=[]}).

to_erlang_term (Digraph) ->
    Vertices = lists:map (fun (V) ->
                                  digraph:vertex (Digraph, V)
                          end, 
                          digraph:vertices (Digraph)),
    Edges = lists:map (fun (E) ->
                               digraph:edge (Digraph, E)
                       end,
                       digraph:edges (Digraph)),
    #digraph{vertices = Vertices,
             edges = Edges}.

from_erlang_term (#digraph{vertices=Vertices, edges=Edges}) ->
    Digraph = digraph:new (),
    lists:foreach (fun ({Vertex, Label}) ->
                           digraph:add_vertex (Digraph, Vertex, Label)
                   end,
                   Vertices),
    lists:foreach (fun ({E, From, To, Label}) ->
                           digraph:add_edge (Digraph, E, From, To, Label)
                   end,
                   Edges),
    Digraph.
