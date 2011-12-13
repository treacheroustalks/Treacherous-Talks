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

-compile([{parse_transform, lager_transform}]).

-include_lib ("utils/include/debug.hrl").
-include_lib ("eunit/include/eunit.hrl").

-export ([get_responding_backend/0,
          get_remote_responding_backend/0,
          get_all_backends/0,
          watch_neighbour/0,
          change_state/2]).


-define(PING_SLEEP, 100).
-define(PING_TRIES, 10).
%% -----------------------------------------------------------------------------
%% @doc
%% `necromancer:watch'es the neighbour this node is responsible for.
%% assumes that `necromancer' is running
%% @end
%% -----------------------------------------------------------------------------
-spec watch_neighbour () -> undefined | node ().
watch_neighbour () ->
    case get_left_neighbour () of
        undefined ->
            undefined;
        Node ->
            necromancer:watch (Node),
            Node
    end.

-spec get_left_neighbour () -> node () | undefined.
get_left_neighbour () ->
    Reverse = lists:reverse (get_all_backends ()),
    get_right_neighbour (node (), Reverse).

%-spec get_right_neighbour () -> node () | undefined.
%get_right_neighbour () ->
%    Nodes = get_all_backends (),
%    get_right_neighbour (node (), Nodes).

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
%% Go through a list of nodes and return the first one that is ping-able. <br/>
%% This functions tries ping all nodes in the Nodes list and returns the
%% first one that returned a pong. In case none returned a pong it will
%% sleep and then repeat this ?PING_TRIES times.
%% @end
%%-------------------------------------------------------------------
-spec get_responding_backend(undefined | [] | [Node]) -> Node | undefined when
      Node :: atom().
get_responding_backend([]) ->
    undefined;
get_responding_backend(undefined) ->
    undefined;
get_responding_backend(Nodes) ->
    responding_backend_fun(Nodes, ?PING_TRIES).

-spec responding_backend_fun([Node], Tries) -> Node | undefined when
      Node :: atom(),
      Tries :: non_neg_integer().
responding_backend_fun(_Nodes, 0) ->
    undefined;
responding_backend_fun(Nodes, Tries) ->
    PingResult = lists:map(fun(Node) ->
                                   {net_adm:ping(Node), Node}
                           end, Nodes),
    Pings = lists:filter(fun({pong,_}) -> true;
                            (_) -> false
                         end, PingResult),
    case Pings of
        [] ->
            timer:sleep(?PING_SLEEP),
            responding_backend_fun(Nodes, Tries-1);
        [{pong, Backend}|_] ->
            Backend
    end.

%% @doc
%% can be called when the backend_nodes value changed.
%% if the left neighbour is not the same after the change, the watch-connection
%% will be exchanged to the new one
%% @end
-spec change_state (atom (), any ()) -> ok.
change_state (backend_nodes, NewList) ->
    OldLeft = get_left_neighbour (),
    application:set_env (controller_app, backend_nodes, NewList),
    NewLeft = get_left_neighbour (),
    if
        OldLeft =/= NewLeft ->
            lager:info("left neighbor changed. was ~p now ~p",
                       [OldLeft, NewLeft]),
            necromancer:unwatch (OldLeft),
            necromancer:watch (NewLeft);
        true ->
            ok
    end;
change_state (_OtherKey, _) ->
    %% don't care..
    ok.

%% @doc
%% returns all known backends in the system
%% @end
-spec get_all_backends () -> [node ()] | [].
get_all_backends () ->
    case application:get_env(controller_app, backend_nodes) of
        {ok, Backends} ->
            Backends;
        _ ->
            []
    end.
