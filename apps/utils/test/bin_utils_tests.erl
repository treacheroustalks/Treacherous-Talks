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
-module(bin_utils_tests).

-include_lib("eunit/include/eunit.hrl").
-import(bin_utils, [tailstr/2,headstr/2,strip/1,bin_rm_trailing_spaces/1,bin_reverse/1]).


% EUnit auto test------------------------------------------------------
tailstr_test_() ->
    ?_assert(tailstr(<<"abcdefg">>,4) == <<"efg">>).

headstr_test_() ->
    ?_assert(headstr(<<"abcdefg">>,4) == <<"abcd">>).

strip_test_() ->
    ?_assert(strip(<<"   hello   ">>) == <<"hello">>).

bin_rm_trailing_spaces_test_() ->
    ?_assert(bin_rm_trailing_spaces(<<"   hello">>) == <<"olleh">>).

bin_reverse_test_() ->
    ?_assert(bin_reverse(<<"abcd">>) == <<"dcba">>).
