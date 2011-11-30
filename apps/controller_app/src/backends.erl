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
%%% @doc provides some convenience functions related to the set of backends
%%%      we have in the system
%%% @end
%%%
%%% @end
%%%-------------------------------------------------------------------

-module (backends).

-include_lib ("utils/include/debug.hrl").
-include_lib ("eunit/include/eunit.hrl").

-export ([get_responding_backend/0,
          get_remote_responding_backend/0,
          get_left_neighbour/0,
          get_right_neighbour/0,
          get_all_backends/0]).

-spec get_left_neighbour () -> node () | undefined.
get_left_neighbour () ->
    Reverse = lists:reverse (get_all_backends ()),
    ?DEBUG ("Reverse: ~p~n", [Reverse]),
    get_right_neighbour (node (), Reverse).

-spec get_right_neighbour () -> node () | undefined.
get_right_neighbour () ->
    Nodes = get_all_backends (),
    get_right_neighbour (node (), Nodes).

get_right_neighbour (_, []) ->
    undefined;
get_right_neighbour (X, Xs) ->
    get_right_neighbour (X, hd (Xs), Xs).

get_right_neighbour (_, _First, []) ->
    undefined;
get_right_neighbour (_X, _First, [_X]) ->
    _First;
get_right_neighbour (_X, _First, [_X | Xs]) ->
    hd (Xs);
get_right_neighbour (X, First, [_ | Xs]) ->
    get_right_neighbour (X, First, Xs).

get_right_neighbour_test () ->
    L = [1,2,3,4],
    ?assertEqual (2, get_right_neighbour (1, hd (L), L)),
    ?assertEqual (1, get_right_neighbour (4, hd (L), L)),
    ?assertEqual (undefined, get_right_neighbour (5, hd (L), L)).

%%-------------------------------------------------------------------
%% @doc
%% Go through the backend_nodes-list in the app file and return the one
%% that is answers to a ping the quickest.
%% @end
%%-------------------------------------------------------------------
-spec get_responding_backend () -> node () | undefined.
get_responding_backend () ->
    get_responding_backend (get_all_backends ()).

%%-------------------------------------------------------------------
%% @doc
%% like {@link get_responding_backend/0} but guarantees that _this_ node is
%% not returned. Obviously it doesn't make a difference if you are using this
%% from a frontend ;-)
%% @end
%%-------------------------------------------------------------------
-spec get_remote_responding_backend () -> node () | undefined.
get_remote_responding_backend () ->
    get_responding_backend (lists:delete (node (), get_all_backends ())).

%%-------------------------------------------------------------------
%% @doc
%% Go through a list of nodes and return the first one that is not equal to
%% `node()' AND that is ping-able. <br/>
%% The implementation is a bit complicated, it roughly works like this:
%% for each of the backends except `node()', a process is spawned that tries to
%% ping it. The first one that responds with "pong" will be the one that is
%% returned. As soon as the quickest responding backend is found, the other
%% ping-processes are killed (since their result is not needed any more).
%% If no backends in the list is reachable, this function takes as long as the
%% slowest ping try takes to return `pang', so be careful.
%% @end
%%-------------------------------------------------------------------
-spec get_responding_backend(undefined | [] | [Node]) -> Node | undefined when
      Node :: atom().
get_responding_backend ([]) ->
    undefined;
get_responding_backend (Nodes) ->
    Self = self (),
    Ref = make_ref (),
    Ping = fun (Node) ->
                   spawn (
                     fun () ->
                             case net_adm:ping (Node) of
                                 pong ->
                                     Self ! {response, Ref, Node, self ()};
                                 pang ->
                                     Self ! {no_response, Ref, Node, self ()}
                             end
                     end)
           end,
    PingProcesses = lists:map (Ping, Nodes),
    response_loop (Ref, PingProcesses).

-spec response_loop (any (), [] | [pid ()]) -> node () | undefined.
response_loop (Ref, PingProcesses) ->
    receive
        {response, Ref, Node, _} ->
            %% kill all the ping process and empty self()'s mailbox:
            lists:foreach (
              fun (PingProcess) ->
                      exit (PingProcess, normal),
                      receive
                          {_, Ref, _, _} ->
                              ok
                      after
                          0 -> ok
                      end
              end,
              PingProcesses),
            Node;
        {no_response, Ref, _, PingProcess} ->
            NewPingProcesses = lists:delete (PingProcess, PingProcesses),
            case NewPingProcesses of
                [] ->
                    undefined;
                NewPingProcesses -> response_loop (Ref, NewPingProcesses)
            end
    end.

%% @doc
%% returns all known backends in the system
%% @end
-spec get_all_backends () -> [node ()] | [].
get_all_backends () ->
    case application:get_env (controller_app, backend_nodes) of
        {ok, Backends} ->
            Backends;
        _ ->
            []
    end.
