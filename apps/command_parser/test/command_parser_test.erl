-module(command_parser_test).
-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}

-include("test_utils.hrl").


parse_test_() ->
    [
     ?_test(check_parse(?SAMPLE_REGISTER,
                        {register, {ok, #user{nick = "Lin", password = "QWER",
                                              email = "ss@lin.pcs", name = "Agner Erlang"}}
                        })),
     ?_test(check_parse(?SAMPLE_UPDATE,
                        {update_user, {ok, "Lin", [{5,"QWER"},{4,field_missing},
                                            {6,"Agner Erlang"}]}
                        })),
     ?_test(check_parse(?SAMPLE_CREATE,
                        {create_game, {ok, #game{name = "awesome_game", press = "white",
                           order_phase = 240, retreat_phase = 210,
                           build_phase = 160, waiting_time = 3200,
                           description = field_missing,
                           password = "1234",
                           num_players = field_missing,
                           creator_id = undefined}}
                        })),
     ?_test(check_parse(?SAMPLE_LOGIN,
                        {login, {ok, #user{nick = "Lin", password = "QWER"}}}))
    ].

check_parse(Sample, Expected) ->
    %io:format(user, "val=~p~n Exp: ~p~n", [command_parser:parse(Sample), Expected]),
    %?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample)).

