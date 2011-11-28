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
%%% @doc Tests the system_state interface.
%%% @end
%%%
%%% @since : 25 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(system_state_tests).

-include_lib("eunit/include/eunit.hrl").


%%-------------------------------------------------------------------
%% Setup applications
%%-------------------------------------------------------------------
apps() ->
    [sasl, os_mon].

app_start() ->
    [application:start(App) || App <- apps()],
    error_logger:tty(false).

app_stop(_) ->
    [application:stop(App) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% Test preparation
%%-------------------------------------------------------------------
controller_test_() ->
    {timeout, 60,
     {setup,
      fun() ->
          app_start()
      end,
      fun app_stop/1,
      [
       ?_test(system_state_get_system_load()),
       ?_test(system_state_get_cpu_usage()),
       ?_test(system_state_get_detailed_cpu_usage()),
       ?_test(system_state_get_memory_usage())
      ]
     }}.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------
system_state_get_system_load() ->
    {Avg1, Avg5, Avg15} = system_state:get_system_load(),
    ?assert(is_integer(Avg1)),
    ?assert(is_integer(Avg5)),
    ?assert(is_integer(Avg15)).

system_state_get_cpu_usage() ->
    ?assertEqual(true, is_float(system_state:get_cpu_usage())).

system_state_get_detailed_cpu_usage() ->
    ?assert({all,0,0,[]} /=
                    system_state:get_detailed_cpu_usage(detailed)),
    ?assert({all,0,0,[]} /=
                    system_state:get_detailed_cpu_usage(per_cpu)).

system_state_get_memory_usage() ->
    ?assertEqual(10, length(system_state:get_memory_usage())).

%%-------------------------------------------------------------------
%% Test data
%%-------------------------------------------------------------------
