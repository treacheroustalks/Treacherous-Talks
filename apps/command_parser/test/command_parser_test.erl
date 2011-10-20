-module(command_parser_test).
-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").
-include("test_utils.hrl").


get_types_test_() ->
    [
     ?_test(check_type(?SAMPLE_REGISTER, register)),
     ?_test(check_type(?SAMPLE_LOGIN, login))
    ].

check_type(Sample, Expected) ->
    ?debugVal(Expected),
    {Result, _} = command_parser:get_type(Sample),
    ?assertEqual(Expected, Result).


parse_test_() ->
    [
     ?_test(check_parse(?SAMPLE_REGISTER,
                        {register, {ok, #user{nick = "Lin", password = "QWER",
                                              email = "ss@pcs", name = "Agner Erlang"}}
                        })),
     ?_test(check_parse(?SAMPLE_LOGIN,
                        {login, {ok, #user{nick = "Lin", password = "QWER"}}}))
    ].

check_parse(Sample, Expected) ->
    ?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample)).
    
