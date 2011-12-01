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

% Define default configuration filename
-define(DEFAULT_CONFIG_FILE, "tt.config").

% Define options list for getopt to use
option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help, $h, "help", undefined, "Show the program options"},
     {setconfig, $c, "setconfig", undefined, "Sets all configurations defined in config file"},
     {ping, $p, "ping", undefined, "Pings all releases defined in config file"},
     {start, $s, "start", undefined, "Starts all releases defined in config file"},
     {stop, $t, "stop", undefined, "Stops all releases defined in config file"},
     {configfile, undefined, undefined, string, "Configuration file (defaults to tt.config if none given)"}
    ].


main(Args) ->
    case getopt:parse(option_spec_list(), Args) of
        {ok, {[], _NonOptionArg}} ->
            usage();
        {ok, {Opts, _NonOptionArg}} ->
            maybe_help(Opts),
            run(Opts);
         {error, _} -> usage()
    end.

run(Opts) ->
    case proplists:get_value(configfile, Opts) of
        undefined -> ConfigFile = ?DEFAULT_CONFIG_FILE;
        Value -> ConfigFile = Value
    end,
    % Get config file
    case file:consult(ConfigFile) of
        {error, enoent} ->
            usage();
        {ok, [Config]} ->
            % http://erlang.2086793.n4.nabble.com/Making-rpc-calls-from-escript-tp2108031p2108033.html
            net_kernel:start([foobar, longnames]),
            erlang:set_cookie(node(), 'treacherous_talks'),
            StartingOrder = cluster_utils:generate_startup_order(Config),
            ProcessedConfig = cluster_utils:preprocess_clustconf(Config),
            case proplists:get_bool(setconfig, Opts) of
                true -> distribute_config(ProcessedConfig);
                false -> ok
            end,
            case proplists:get_bool(start, Opts) of
                true ->
                    do_action_on_releases(StartingOrder, start_release),
                    cluster_utils:notify_backends(Config);
                false -> ok
            end,
            case proplists:get_bool(stop, Opts) of
                true ->
                    ShutdownOrder = lists:reverse(StartingOrder),
                    do_action_on_releases(ShutdownOrder, stop_release);
                false -> ok
            end,
            case proplists:get_bool(ping, Opts) of
                true -> do_action_on_releases(StartingOrder, ping_release);
                false -> ok
            end
    end.

distribute_config([]) -> ok;
distribute_config([{host, Host, HostConfig}|Rest]) ->
    Node = list_to_atom("system_manager@" ++ Host),
    % Try to ensure connection but don't care about the return value since
    % rpc:call will figure that one out anyway...
    net_adm:ping(Node),
    Res = (catch rpc:call(Node, system_manager, update_config, [{host, Host, HostConfig}])),
    io:format ("d_cfg: ~p~n", [{host, Host, HostConfig}]),
    io:format("update_config ~p on ~p was ~p~n", [HostConfig, Node, Res]),
    distribute_config(Rest).

%% Perform an action (start/stop/ping) on all releases in the given list
-spec do_action_on_releases(list(), atom()) ->
    ok | {error, term()} | {badrpc, term()}.
do_action_on_releases([], _Action) -> ok;
do_action_on_releases([{Host, Release}|Rest], Action) ->
    Node = list_to_atom("system_manager@" ++ Host),
    % Try to ensure connection but don't care about the return value since
    % rpc:call will figure that one out anyway...
    net_adm:ping(Node),
    Res = (catch rpc:call(Node, system_manager, Action, [Release])),
    io:format("~p ~p on ~p was ~p~n", [Action, Release, Node, Res]),
    do_action_on_releases(Rest, Action).

%% Getopt helpers
usage() ->
    usage(option_spec_list()).

usage(OptSpecList) ->
    getopt:usage(OptSpecList, "cluster_manager"),
    halt(127).

maybe_help(Opts) ->
  case proplists:get_bool(help, Opts) of
      true -> usage();
      false -> ok
  end.
