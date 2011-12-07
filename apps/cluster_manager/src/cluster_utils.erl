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
%%% @doc Utility functions for management of an entire distributed TT cluster.
%%% @end
%%%
%%% @since : 24 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(cluster_utils).

-include_lib("datatypes/include/clusterconf.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([generate_startup_order/1, preprocess_clustconf/1, notify_backends/1]).
-export([distribute_config/1, do_action_on_releases/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

distribute_config([]) -> [];
distribute_config([{host, Host, SysMgr, HostConfig}|Rest]) ->
    Node = list_to_atom(SysMgr ++ "@" ++ Host),
    % Try to ensure connection but don't care about the return value since
    % rpc:call will figure that one out anyway...
    net_adm:ping(Node),
    Res = (catch rpc:call(Node, system_manager, update_config, [{host, Host, HostConfig}])),
    print_release_action_result({update_config, Host, Res}),
    [{update_config, Host, Res} | distribute_config(Rest)].


%% Perform an action (start/stop/ping/halt) on all releases in the given list
-spec do_action_on_releases(list(), atom()) ->
    ok | {error, term()} | {badrpc, term()}.
do_action_on_releases([], _Action) -> [];
do_action_on_releases([{Host, SysMgr, Release, _RelPrefix}|Rest], Action) ->
    Node = list_to_atom(SysMgr ++ "@" ++ Host),
    % Try to ensure connection but don't care about the return value since
    % rpc:call will figure that one out anyway...
    net_adm:ping(Node),
    Res = (catch rpc:call(Node, system_manager, Action, [Release])),
    print_release_action_result({Action, Release, Host, Res}),
    [{Action, Release, Host, Res} | do_action_on_releases(Rest, Action)].


%% @doc
%%  Generate a startup order from a given clustconf()
%%  which simply puts any riak release before any backend, and any
%%  backend before anything else.
%%
%%  For now, two or more of the same release types will be returned in
%%  the order they occurred in the clustconf() but this should not
%%  be relied upon.
%% @end
-spec generate_startup_order(clustconf()) -> [{hostname(), relname()}].
generate_startup_order(ClustConf) ->
    lists:sort(fun startup_sort/2, get_releases(ClustConf)).

%% @doc
%%  Adds an variable backend_nodes with a list of node names
%%  to a hardocded list of applications in a hardcoded list of
%%  releases.
%%  Returns the modified clustconf.
%% @end
-spec preprocess_clustconf(clustconf()) -> clustconf().
preprocess_clustconf(ClustConfig) ->
    BackendNodes = config_backend_nodes(ClustConfig),
    % Pass BackendNodes along inside Acc. The right way is to generate the Fun
    % containing the static data and have Acc be a pure accumulator.
    {_, ProcessedConfig} = lists:foldl(fun add_backend_nodes/2,
                                       {BackendNodes, ClustConfig},
                                       ClustConfig),
    ProcessedConfig.

%% @doc
%%  Contacts all Backends and calls `backends:change_state/2' so that
%%  the watch-ring stays intact.
%%
%%  Assumes one backend per host.
%% @end
notify_backends(ClustConf) ->
    IsBackend = fun({_, _, Relname, _}) ->
                        Relname =:= backend
                end,
    BackendOrders = lists:filter(IsBackend, generate_startup_order (ClustConf)),
    OrderToNode = fun({Hostname, _SysMgr, _Relname, RelPrefix}) ->
                          list_to_atom(
                            atom_to_list(RelPrefix) ++ "@" ++ Hostname)
                     end,
    Backends = lists:map (OrderToNode,BackendOrders),
    InformOfBackends =
        fun(Backend) ->
                Res =
                    (catch
                         rpc:call(Backend,
                                  backends,
                                  change_state,
                                  [backend_nodes, Backends])),
                io:format ("informing ~p, result was: ~p~n", [Backend, Res])
        end,
    lists:foreach(InformOfBackends, Backends).


%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

print_release_action_result({Action, Release, Host, Res}) ->
    io:format("~p ~p on ~p was ~p~n", [Action, Release, Host, Res]);
print_release_action_result({Action, Host, Res}) ->
    io:format("~p on ~p was ~p~n", [Action, Host, Res]).


% Riak comes before anything.
startup_sort({_, _,riak,_}, _Anything) ->
    true;
% Riak comes after nothing.
startup_sort(_Anything, {_, _, riak,_}) ->
    false;
% Backend comes before anything but riak.
startup_sort({_, _, backend,_}, _Anything) ->
    true;
% Backend comes after nothing but riak.
startup_sort(_Anything, {_, _,backend,_}) ->
    false;
% Anything else is equal.
startup_sort(_, _) ->
    true.

get_releases([]) -> [];
get_releases([{host, Host, SysMgrPrefix, RelConfs}|Rest]) ->
    host_releases(Host, SysMgrPrefix, RelConfs) ++ get_releases(Rest).

host_releases(_Host, _SysMgrPrefix, []) -> [];
host_releases(Host, SysMgrPrefix, [{release, Relname, RelPrefix, _RelConf}|Rest]) ->
    [{Host, SysMgrPrefix, Relname, RelPrefix} |
     host_releases(Host, SysMgrPrefix, Rest)].

% replace hostconf with a hostconf where the game and controller apps in the
% backend release have been given a backend node list.
% HostConf is one of the elements in ClustConf
% This is intended for use in a lists:foldl over an entire ClustConf.
add_backend_nodes(HostConf, {BackendNodes, ClustConf}) ->
    %% repetition because I can't handle another lists:foldl
    {_, NewClustConf1} = add_backend_nodes(backend, [controller_app, game],
                                         HostConf, {BackendNodes, ClustConf}),
    {_, NewClustConf2} = add_backend_nodes(web_frontend, [controller_app],
                                         HostConf, {BackendNodes, NewClustConf1}),
    {_, NewClustConf3} = add_backend_nodes(smtp_frontend, [controller_app],
                                         HostConf, {BackendNodes, NewClustConf2}),
    {_, NewClustConf4} = add_backend_nodes(xmpp_frontend, [controller_app],
                                         HostConf, {BackendNodes, NewClustConf3}),
    {BackendNodes, NewClustConf4}.

-spec add_backend_nodes(atom(), [atom()], hostconf(),
                        {[atom()], clustconf()}) -> {[atom()], clustconf()}.
add_backend_nodes(ToRelease,
                  ToApps,
                  {host, Host, SysMgr, _DontUseThisHostConf},
                  {BackendNodes, ClustConf}) ->
    % Use HostConf from the accumulator, NOT the one given.
    % Just use the Host from the given HostConf to track
    % where we are in the loop.
    {host, Host, SysMgr, RelConfs} = lists:keyfind(Host, 2, ClustConf),
    case lists:keyfind(ToRelease, 2, RelConfs) of
        false ->
            {BackendNodes, ClustConf};
        {release, ToRelease, RelPrefix, AppConfs} ->
            ConfigMods = backend_nodes_conf_changes(ToApps, BackendNodes),
            NewAppConfs = manage_config:update_config(AppConfs, ConfigMods),
            NewRelConf = {release, ToRelease, RelPrefix, NewAppConfs},
            NewRelConfs = lists:keyreplace(ToRelease, 2, RelConfs, NewRelConf),
            NewClustConf = lists:keyreplace(Host, 2, ClustConf,
                                            {host, Host, SysMgr, NewRelConfs}),
            {BackendNodes, NewClustConf}
    end.

backend_nodes_conf_changes([], _) -> [];
backend_nodes_conf_changes([App|Rest], BackendNodes) ->
    [{App, [{backend_nodes, BackendNodes}]}| backend_nodes_conf_changes(Rest, BackendNodes)].


% Given a clustconf(), it returns a list of node atoms for expected backend nodes
-spec config_backend_nodes(clustconf()) -> [atom()].
config_backend_nodes(Config) ->
    lists:flatten(lists:map(fun host_conf_backend_node/1, Config)).

-spec host_conf_backend_node(hostconf()) -> [] | atom().
host_conf_backend_node({host, Host, _SysMgr, Rels}) ->
    case lists:keyfind(backend, 2, Rels) of
        false ->
            [];
        {release, backend, RelPrefix, _} ->
            list_to_atom(atom_to_list(RelPrefix) ++ "@" ++ Host)
    end.
