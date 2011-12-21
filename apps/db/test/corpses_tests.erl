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
%%% @doc Unit tests for corpses.
%%% @end
%%%
%%% @since : 18 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(corpses_tests).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [protobuffs, riakc, db].

app_start() ->
    error_logger:tty(false),
    [ ?assertEqual(ok, application:start(App)) || App <- apps()].

app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% All corpses tests
%%-------------------------------------------------------------------
corpses_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     [save_and_get_tst()]
    }.
%%-------------------------------------------------------------------
save_and_get_tst() ->
    {setup,
     fun() -> % setup
             ?debugMsg("Starting corpses interface test"),
             Node = 'some_name@999.999.999.999',
             os:cmd("epmd -daemon"), % net_kernel needs epmd.
             net_kernel:start([Node, longnames]),
             Mod = ?MODULE,
             Ref1 = 1,
             Data1 = {some, data, 1},
             corpses:save_corpse(Mod, Ref1, Data1),

             Ref2 = 2,
             Data2 = {some, data, 2},
             corpses:save_corpse(Mod, Ref2, Data2),
             {Node, Mod, [Data1, Data2], [Ref1, Ref2]}
     end,
     fun({Node, Mod, _Data, Refs}) -> % teardown
             lists:foreach(
               fun(Ref) ->
                       corpses:delete_corpse(Node, Mod, Ref)
               end, Refs)
     end,
     fun({Node, _Mod, Expected, _Refs}) -> % test
             [fun() ->
                      Corpses = corpses:get_corpses(Node),

                      ?assertEqual(length(Expected), length(Corpses)),
                      lists:foreach(
                        fun({_, {_Key, Val}}=Corpse) ->
                                ?debugVal(Corpse),
                                ?assert(lists:member(Val, Expected))
                        end, Corpses),
                      ?debugMsg("Corpses interface test successful")
              end]
     end}.
