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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Provides config functionality that is used by all service
%%% applications.
%%% @end
%%%
%%% @since : 14 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(service_conf).

%% Public interface
-export([node_pids/1, node_count/1, worker_count/1, worker_count/2]).


%%-------------------------------------------------------------------
%% @doc
%% Returns the pids of all the config processes for a given service.
%% @end
%% @spec node_pids(Name::atom()) -> integer()
%% @end
%%-------------------------------------------------------------------
node_pids(Name) ->
    pg2:get_members(Name).

%%-------------------------------------------------------------------
%% @doc
%% Returns the number of running config processes, thus the number of
%% nodes running the service.
%% @spec node_count(Name::atom()) -> integer()
%% @end
%%-------------------------------------------------------------------
-spec node_count(atom()) -> integer().
node_count(Name) ->
    length(node_pids(Name)).

%%-------------------------------------------------------------------
%% @doc
%% Queries the number of workers on a node from the given Pid.
%% @end
%% @spec worker_count(Pid::pid()) -> integer()
%% @end
%%-------------------------------------------------------------------
-spec worker_count(pid()) -> integer().
worker_count(Pid) ->
    gen_server:call(Pid, worker_count).

%%-------------------------------------------------------------------
%% @doc
%% Changes the number of workers on the node where Pid lives, 
%% and returns the new number.
%% @end
%% @spec worker_count(Pid::pid(), Count::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
-spec worker_count(pid(), integer()) -> integer().
worker_count(Pid, Count) ->
    gen_server:call(Pid, {worker_count, Count}).

