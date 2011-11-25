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

-module(system_utils).

-include("include/sysconfig.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([generate_startup_order/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc
%%  Generate a startup order from a given sysconf()
%%  which simply puts any riak release before any backend, and any
%%  backend before anything else.
%%
%%  For now, two or more of the same release types will be returned in
%%  the order they occurred in the sysconf() but this should not
%%  be relied upon.
%% @end
-spec generate_startup_order(sysconf()) -> [{hostname(), relname()}].
generate_startup_order(SysConf) ->
    lists:sort(fun startup_sort/2, get_releases(SysConf)).

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

% Riak comes before anything.
startup_sort({_,riak}=A, B=_Anything) ->
    io:format("~p ~p~n", [A,B]),
    true;
% Riak comes after nothing.
startup_sort(_Anything=A, B={_, riak}) ->
    io:format("~p ~p~n", [A,B]),
    false;
% Backend comes before anything but riak.
startup_sort({_, backend}=A, B=_Anything) ->
    io:format("~p ~p~n", [A,B]),
    true;
% Backend comes after nothing but riak.
startup_sort(_Anything=A, B={_,backend}) ->
    io:format("~p ~p~n", [A,B]),
    false;
% Anything else is equal.
startup_sort(_=A, B=_) ->
    io:format("~p ~p~n", [A,B]),
    true.

get_releases([]) -> [];
get_releases([{host, Host, RelConfs}|Rest]) ->
    host_releases(Host, RelConfs) ++ get_releases(Rest).

host_releases(_Host, []) -> [];
host_releases(Host, [{release, Relname, _RelConf}|Rest]) ->
    [{Host, Relname}|host_releases(Host, Rest)].
