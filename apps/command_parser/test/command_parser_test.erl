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
                        {update_user, {ok, 123456,
                                       [{#user.password,"QWER"},
                                        {#user.email,field_missing},
                                        {#user.name,"Agner Erlang"}]}
                        })),
     ?_test(check_parse(?SAMPLE_CREATE,
                        {create_game, {ok, 987654,
                                       #game{name = "awesome_game", press = "white",
                                             order_phase = 240, retreat_phase = 210,
                                             build_phase = 160, waiting_time = 3200,
                                             description = field_missing,
                                             password = "1234",
                                             num_players = field_missing,
                                             creator_id = undefined}}
                        })),
     ?_test(check_parse(?SAMPLE_LOGIN,
                        {login, {ok, #user{nick = "Lin", password = "QWER"}}})),
     ?_test(check_parse(?SAMPLE_GAME_OVERVIEW,
                        {game_overview, {ok, 123456789, 111222}})),
     ?_test(check_parse(?SAMPLE_JOIN_GAME,
                        {join_game, {ok, 123456789, 111222, england}}))
    ].

check_parse(Sample, Expected) ->
    %io:format(user, "val=~p~n Exp: ~p~n", [command_parser:parse(Sample), Expected]),
    %?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample)).


reconfig_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_RECONFIG),
    Expected = {reconfig_game,{ok,456123,111222,
                   [{4,"awesome_game"},
                    {7,"white"},
                    {8,240},
                    {9,210},
                    {10,160},
                    {14,3200},
                    {5,field_missing},
                    {11,field_missing},
                    {12,"1234"},
                    {3,undefined}]}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

order_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_TEST_ORDERS1),
    Expected = {player_orders,{ok,{[{move,fleet,mid,nat,north_coast},
                     {waive},
                     {build,fleet,mun,north_coast},
                     {remove,army,mun},
                     {disband,army,mun},
                     {support_move,army,mun,fleet,kie,ber,any_coast},
                     {support_move,army,mun,fleet,kie,ber,any_coast},
                     {support_hold,fleet,fin,any_unit,bre},
                     {support_hold,fleet,fin,any_unit,bre},
                     {hold,any_unit,hol},
                     {hold,army,bre},
                     {hold,army,bre},
                     {convoy,fleet,gol,army,bre,mar},
                     {convoy,fleet,wes,army,bre,mar},
                     {convoy,fleet,fin,army,bre,mar},
                     {move,army,bre,mar,any_coast},
                     {convoy,fleet,gol,army,bre,mar},
                     {convoy,fleet,wes,army,bre,mar},
                     {convoy,fleet,fin,army,bre,mar},
                     {move,army,bre,mar,any_coast},
                     {convoy,fleet,nth,army,lon,nrg},
                     {convoy,fleet,nth,army,lon,nrg},
                     {move,army,lon,nrg,any_coast},
                     {move,army,lon,nrg,north_coast},
                     {move,any_unit,lon,nrg,any_coast},
                     {move,army,lon,nrg,any_coast}],
                    [{error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",["a","bre","->","f",[],[],[]]}},
                     {error,{"invalid action#",["a","bre","-","f",[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
                     {error,"eid#invalid location name, not in atom table"},
                     {error,"ses#invalid location name, not in atom table"}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].
