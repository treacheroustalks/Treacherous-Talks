-module(bin_utils).
-export([tailstr/2, headstr/2, strip/1]).

%Export for eunit test
-export([bin_rm_trailing_spaces/1, bin_reverse/1]).

%%------------------------------------------------------------------------------------------------
%% @function tailstr/1 @end
%%
%% @doc Split a bin string into two parts, return the right part.
%%------------------------------------------------------------------------------------------------
tailstr(BinStr, {Head, Len}) ->
    N = Head + Len,
    tailstr(BinStr, N);
tailstr( BinStr, N) ->
    StrSize = byte_size(BinStr),
    binary:part(BinStr, N, StrSize - N).

%%------------------------------------------------------------------------------------------------
%% @function headstr/1 @end
%%
%% @doc Split a bin string into two parts, return the left part.
%%------------------------------------------------------------------------------------------------
headstr(BinStr, {N, _}) ->
    headstr(BinStr, N);
headstr(BinStr, N) ->
    binary:part(BinStr, 0, N).

%%------------------------------------------------------------------------------------------------
%% @function strip/1 @end
%%
%% @doc Remove leading and trailing spaces of a binary string.
%%------------------------------------------------------------------------------------------------
strip(<<" ",Rest/binary>>) ->
    strip(Rest);
strip(WithoutLeadingSpaces) ->
    bin_rm_trailing_spaces(bin_reverse(WithoutLeadingSpaces)).


%=======Internal Functions===============================================
%%------------------------------------------------------------------------------------------------
%% @function bin_rm_trailing_spaces/1 @end
%%
%% @doc Remove trailing spaces of a bin string then reverse it
%%------------------------------------------------------------------------------------------------
bin_rm_trailing_spaces(<<" ",Rest/binary>>) ->
    bin_rm_trailing_spaces(Rest);
bin_rm_trailing_spaces(WithoutTrailing) ->
    bin_reverse(WithoutTrailing).

%%------------------------------------------------------------------------------------------------
%% @function bin_reverse/1 @end
%%
%% @doc Reverse a binary string.
%%------------------------------------------------------------------------------------------------
bin_reverse(Bin) ->
    Size = bit_size(Bin),
    <<T:Size/integer-little>> = Bin,
    <<T:Size/integer-big>>.
