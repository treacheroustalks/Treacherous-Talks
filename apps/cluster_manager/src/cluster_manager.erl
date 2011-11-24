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
%%% @doc escript for management of an entire distributed TT cluster
%%%      from the console.
%%% @end
%%%
%%% @since : 24 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(cluster_manager).

-include_lib("datatypes/include/clusterconf.hrl").

-export([main/1]).

main([]) ->
    main(["tt.config"]);

main([ConfigPath]) ->
    case file:consult(ConfigPath) of
        {error, enoent} ->
            usage();
        {ok, [Config]} ->
            StartingOrder = cluster_utils:generate_startup_order(Config),
            io:format("Config: ~p~n"
                      "Starting order:~p~n",
                      [Config, StartingOrder]),
            ProcessedConfig = cluster_utils:preprocess_clustconf(Config),
            io:format("Processed config ~p~n", [ProcessedConfig]),
            distribute_config(ProcessedConfig),
            start_releases(StartingOrder)
    end.

distribute_config([]) -> ok;
distribute_config([{host, Host, HostConfig}|Rest]) ->
    Node = "system_manager@" ++ Host,
    io:format("rpc:call( ~s, system_manager, update_config, ~p )~n",
              [Node, HostConfig]),
    distribute_config(Rest).

start_releases([]) -> ok;
start_releases([{Host, Release}|Rest]) ->
    Node = "system_manager@" ++ Host,
    io:format("rpc:call( ~s, system_manager, start_release, ~p )~n",
              [Node, Release]),
    start_releases(Rest).


usage() ->
    io:format("usage: cluster_manager [<system config path>]"
              " (default tt.config)~n", []),
    halt(1).
