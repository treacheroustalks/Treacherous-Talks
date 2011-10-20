-module(command_parser_test).
-include_lib("eunit/include/eunit.hrl").

-include("test_utils.hrl").


get_type_test_() ->
    Output = register,
    ?_test(?assertEqual(Output, command_parser:get_type(?SAMPLE_REGISTER))).
