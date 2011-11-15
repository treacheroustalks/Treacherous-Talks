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
-module(service_tests).
-include_lib("eunit/include/eunit.hrl").

service_worker_test() ->
    Group = test_worker,

    service_worker:join_group(Group),
    ?assertEqual([self()], service_worker:get_members(Group)),
    
    service_worker:join_group(Group),
    ?assertEqual([self(), self()], service_worker:get_members(Group)),

    ?assertEqual(self(), service_worker:select_pid(Group)).
    

service_conf_test() ->
    Group = test_conf,

    service_worker:join_group(Group),
    ?assertEqual([self()], service_conf:node_pids(Group)),

    service_worker:join_group(Group),
    ?assertEqual([self(), self()], service_conf:node_pids(Group)),

    ?assertEqual(2, service_conf:node_count(Group)).
