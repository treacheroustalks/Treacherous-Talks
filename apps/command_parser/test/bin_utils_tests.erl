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
