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
-module(bin_utils).
-export([tailstr/2, headstr/2, strip/1]).

%Export for eunit test
-export([bin_rm_trailing_spaces/1, bin_reverse/1]).

%%------------------------------------------------------------------------------
%% @doc tailstr/1
%%
%% Split a bin string into two parts, return the right part.
%% @end
%-------------------------------------------------------------------------------
-spec tailstr(BinStr::binary(), integer() | term()) -> binary().
tailstr(BinStr, {Head, Len}) ->
    N = Head + Len,
    tailstr(BinStr, N);
tailstr(BinStr, N) ->
    StrSize = byte_size(BinStr),
    binary:part(BinStr, N, StrSize - N).


%%------------------------------------------------------------------------------
%% @doc headstr/1
%%
%% Split a bin string into two parts, return the left part.
%% @end
%%------------------------------------------------------------------------------
-spec headstr(BinStr::binary(), integer() | term()) -> binary().
headstr(BinStr, {N, _}) ->
    headstr(BinStr, N);
headstr(BinStr, N) ->
    binary:part(BinStr, 0, N).


%%------------------------------------------------------------------------------
%% @doc strip/1
%%
%% Remove leading and trailing spaces of a binary string.
%% @end
%%------------------------------------------------------------------------------
-spec strip(binary()) -> binary().
strip(<<" ",Rest/binary>>) ->
    strip(Rest);
strip(WithoutLeadingSpaces) ->
    bin_rm_trailing_spaces(bin_reverse(WithoutLeadingSpaces)).


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc bin_rm_trailing_spaces/1
%%
%% Remove trailing spaces of a bin string then reverse it
%% @end
%%------------------------------------------------------------------------------
bin_rm_trailing_spaces(<<" ",Rest/binary>>) ->
    bin_rm_trailing_spaces(Rest);
bin_rm_trailing_spaces(WithoutTrailing) ->
    bin_reverse(WithoutTrailing).


%%------------------------------------------------------------------------------
%% @doc bin_reverse/1
%%
%% Reverse a binary string.
%% @end
%%------------------------------------------------------------------------------
bin_reverse(Bin) ->
    Size = bit_size(Bin),
    <<T:Size/integer-little>> = Bin,
    <<T:Size/integer-big>>.
