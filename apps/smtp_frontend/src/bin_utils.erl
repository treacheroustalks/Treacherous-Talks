-module(bin_utils).
-export([tailstr/2, headstr/2, strip/1]).

%Export for eunit test
-export([bin_rm_trailing_spaces/1, bin_reverse/1]).

%%------------------------------------------------------------------------------------------------
%% @doc tailstr/1
%%
%% Split a bin string into two parts, return the right part.
%% @end
%------------------------------------------------------------------------------------------------
tailstr(BinStr, {Head, Len}) ->
    N = Head + Len,
    tailstr(BinStr, N);
tailstr(BinStr, N) ->
    StrSize = byte_size(BinStr),
    binary:part(BinStr, N, StrSize - N).


%%------------------------------------------------------------------------------------------------
%% @doc headstr/1
%%
%% Split a bin string into two parts, return the left part.
%% @end
%%------------------------------------------------------------------------------------------------
headstr(BinStr, {N, _}) ->
    headstr(BinStr, N);
headstr(BinStr, N) ->
    binary:part(BinStr, 0, N).


%%------------------------------------------------------------------------------------------------
%% @doc strip/1
%%
%% Remove leading and trailing spaces of a binary string.
%% @end
%%------------------------------------------------------------------------------------------------
strip(<<" ",Rest/binary>>) ->
    strip(Rest);
strip(WithoutLeadingSpaces) ->
    bin_rm_trailing_spaces(bin_reverse(WithoutLeadingSpaces)).


%=======Internal Functions===============================================
%%------------------------------------------------------------------------------------------------
%% @doc bin_rm_trailing_spaces/1
%%
%% Remove trailing spaces of a bin string then reverse it
%% @end
%%------------------------------------------------------------------------------------------------
bin_rm_trailing_spaces(<<" ",Rest/binary>>) ->
    bin_rm_trailing_spaces(Rest);
bin_rm_trailing_spaces(WithoutTrailing) ->
    bin_reverse(WithoutTrailing).


%%------------------------------------------------------------------------------------------------
%% @doc bin_reverse/1
%%
%% Reverse a binary string.
%% @end
%%------------------------------------------------------------------------------------------------
bin_reverse(Bin) ->
    Size = bit_size(Bin),
    <<T:Size/integer-little>> = Bin,
    <<T:Size/integer-big>>.
