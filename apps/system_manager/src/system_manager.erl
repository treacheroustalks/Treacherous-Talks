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
%%% @author Erik Timan <erol7391@student.uu.se>
%%%
%%% @doc The system manager application handles configuration of the Treacherous
%%% Talks system. It is intended to be the single point of entry for configuring
%%% and controlling the whole system on a server.
%%%
%%% @end
%%%
%%% @since : 23 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(system_manager).

-export([update_config/1,
         start_release/1,
         stop_release/1,
         ping_release/1,
         join_riak/1
        ]).

-include_lib("datatypes/include/clusterconf.hrl").

%% Set a timeout for API calls.
-define(TIMEOUT, 35000).

%% ------------------------------------------------------------------
%% Internal Macro Definitions
%% ------------------------------------------------------------------
-define(WORKER, system_manager_worker).
-define(CALL_WORKER(Cmd), try gen_server:call(
                                service_worker:select_pid(?WORKER),
                                Cmd,
                                ?TIMEOUT)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc
%% Update all the config files needed to fulfill the given host configuration
%% tuple.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_config(hostconf()) -> ok | {error, term()}.
update_config(HostConfig) ->
    ?CALL_WORKER({update_config, HostConfig}).

%% ------------------------------------------------------------------
%% @doc
%% Starts a given release and returns as soon as it is possible to ping the
%% VM. Please notice that this does not ensure that any applications has
%% started correctly.
%%
%% @end
%% ------------------------------------------------------------------
-spec start_release(relname()) -> ok | {error, term()}.
start_release(Relname) ->
    ?CALL_WORKER({start_release, Relname}).

%% ------------------------------------------------------------------
%% @doc
%% Stops a given release and returns only when the release has been truly
%% stopped (VM is shutdown).
%%
%% @end
%% ------------------------------------------------------------------
-spec stop_release(relname()) -> ok | {error, term()}.
stop_release(Relname) ->
    ?CALL_WORKER({stop_release, Relname}).

%% ------------------------------------------------------------------
%% @doc
%% Pings a given release and returns the result.
%%
%% @end
%% ------------------------------------------------------------------
-spec ping_release(relname()) -> up | down | {error, term()}.
ping_release(Relname) ->
    ?CALL_WORKER({ping_release, Relname}).

%% ------------------------------------------------------------------
%% @doc
%% Adds the local Riak node to a cluster by sending a join request to the given
%% Node.
%%
%% @end
%% ------------------------------------------------------------------
-spec join_riak(hostname()) -> ok | {error, term()}.
join_riak(Node) ->
    ?CALL_WORKER({join_riak, Node}).
