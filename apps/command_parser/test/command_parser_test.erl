-module(command_parser_test).
-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}

-include("test_utils.hrl").


get_types_test_() ->
    [
     ?_test(check_type(?SAMPLE_REGISTER, register)),
     ?_test(check_type(?SAMPLE_UPDATE, update_user)),
     ?_test(check_type(?SAMPLE_LOGIN, login)),
     ?_test(check_type(?SAMPLE_CREATE, create_game))
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
     ?_test(check_parse(?SAMPLE_UPDATE,
                        {update_user, {ok, #user{nick = "Lin", password = "QWER",
                                              name = "Agner Erlang"}}
                        })),
     ?_test(check_parse(?SAMPLE_CREATE, 
                        {create_game, {ok, #game{
                                         name = "awesome_game", press = "white",
                                         order_phase = "4H", retreat_phase = "3H30M",
                                         build_phase = "2H40M", waiting_time = "2D5H20M",
                                         creator_id = undefined}}})),
     ?_test(check_parse(?SAMPLE_LOGIN,
                        {login, {ok, #user{nick = "Lin", password = "QWER"}}}))
    ].

check_parse(Sample, Expected) ->
    ?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample)).

