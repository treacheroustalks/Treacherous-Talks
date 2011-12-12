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
%% -----------------------------------------------------------------------------
%% @doc
%% The module is responsible for serializing/deserializing a digraph
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
%% @TODO: Add specs
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
