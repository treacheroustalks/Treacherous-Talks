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
%%% @author Dilshod Aliev <Dilshod.Aliev.1565@student.uu.se>
%%%
%%% @doc System state. Monitors the system's state.
%%%
%%% Several functions from os_mon application have been used in this
%%% module.
%%% @reference See
%%% <a href="http://www.erlang.org/doc/man/os_mon_app.html">
%%% OS_MON</a>
%%% for more information.
%%% @end
%%%
%%% @since : 24 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(system_state).

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------
-export([
         get_system_load/0,
         get_cpu_usage/0,
         get_detailed_cpu_usage/1,
         get_memory_usage/0
        ]).


%% ------------------------------------------------------------------
%% Interface Function Implementation
%% ------------------------------------------------------------------

%%%------------------------------------------------------------------
%% @doc
%% Gets the average system load in the last 1, 5 and 15 minutes
%%
%% @spec get_system_load() ->
%%           {integer(), integer(), integer()} |
%%           {{error, Reason}, {error, Reason}, {error, Reason}}
%% @end
%%%------------------------------------------------------------------
get_system_load() ->
    SystemLoad = {cpu_sup:avg1(), cpu_sup:avg5(), cpu_sup:avg15()},
    SystemLoad.

%%%------------------------------------------------------------------
%% @doc
%% Gets CPU usage information
%%
%% @spec get_cpu_usage() -> float() | {error, Reason}
%% @end
%%%------------------------------------------------------------------
get_cpu_usage() ->
    CpuUsage = cpu_sup:util(),
    CpuUsage.

%%%------------------------------------------------------------------
%% @doc
%% Gets more detailed CPU usage information than get_cpu_usage()
%%
%% @spec get_detailed_cpu_usage(detailed | per_cpu) ->
%%           tuple() | list() | {error, Reason}
%% @end
%%%------------------------------------------------------------------
get_detailed_cpu_usage(Opt) ->
    CpuUsage = cpu_sup:util([Opt]),
    CpuUsage.

%%%------------------------------------------------------------------
%% @doc
%% Gets memory usage information.
%% It returns a list of tuples with atoms and values, where atoms
%% are short descriptions of particular types of memory
%% (except pid and alloc_pid, which are process id and number of
%% allocated bytes of the largest Erlang process on the node, respectively),
%% and values are integers that present memory sizes as number of bytes,
%% example:
%%
%% [{pid, Pid},
%%  {alloc_pid, integer()},
%%  {alloc_memory, integer()},
%%  {system_total_memory,integer()},
%%  {free_swap,integer()},
%%  {total_swap,integer()},
%%  {cached_memory,integer()},
%%  {buffered_memory,integer()},
%%  {free_memory,integer()},
%%  {total_memory,integer()}] = get_memory_usage()
%%
%% get_memory_usage() is a combination of memsup:get_memory_data()
%% and memsup:get_system_memory_data functions which are part of the
%% os_mon application.
%%
%% @spec get_memory_usage() -> list() | {error, Reason}
%% @end
%%%------------------------------------------------------------------
get_memory_usage() ->
    {_TotalMem, AllocatedMem, {Pid, PidAllocated}} = memsup:get_memory_data(),
    MemoryData = [{pid, Pid},
                  {alloc_pid, PidAllocated},
                  {alloc_memory, AllocatedMem}],
    SystemMemoryData = memsup:get_system_memory_data(),
    MemoryData ++ SystemMemoryData.
