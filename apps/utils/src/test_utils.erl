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
%%% @author Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc some function that could be used in unit test
%%%
%%% @since : 13 Dec 2011 by Bermuda Triangle
%%%==================================================================
-module(test_utils).

-export([wait_for_change/3]).

%%------------------------------------------------------------------------------
%% @doc
%%  get a function, initial value and number of interation
%%  it retruns the updated value if value is changed otherwise return intial value
%% @end
%%------------------------------------------------------------------------------
wait_for_change (_, Initial, 0) ->
    Initial;
wait_for_change (Fun, Initial, Tries) ->
    case Fun () of
        Initial ->
            timer:sleep (10),
            wait_for_change (Fun, Initial, Tries -1);
        Changed ->
            Changed
    end.