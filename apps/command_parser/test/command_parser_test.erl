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
                        {update_user, {ok, "123456",
                                       [{#user.password,"QWER"},
                                        {#user.email,field_missing},
                                        {#user.name,"Agner Erlang"}]}
                        })),
     ?_test(check_parse(?SAMPLE_CREATE,
                        {create_game, {ok, "987654",
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
                        {game_overview, {ok, "123456789", 111222}})),
     ?_test(check_parse(?SAMPLE_JOIN_GAME,
                        {join_game, {ok, "123456789", {111222, england}}}))
    ].

check_parse(Sample, Expected) ->
    %io:format(user, "val=~p~n Exp: ~p~n", [command_parser:parse(Sample), Expected]),
    %?debugVal(Expected),
    ?assertEqual(Expected, command_parser:parse(Sample)).


reconfig_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_RECONFIG),
    Expected = {reconfig_game,{ok,"456123", {
                                    111222,
                                    [{4,"awesome_game"},
                                     {7,"white"},
                                     {8,240},
                                     {9,210},
                                     {10,160},
                                     {14,3200},
                                     {5,field_missing},
                                     {11,field_missing},
                                     {12,"1234"},
                                     {3,field_missing}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].

order_test_() ->
    ActualOutput = command_parser:parse(?SAMPLE_TEST_ORDERS1),
    Expected = {game_move,{ok,{session,1234341},
    {gameid,3958230945903},
    {[{move,fleet,mid_atlantic_ocean,north_atlantic_ocean,north_coast},
      {waive},
      {build,fleet,munich,north_coast},
      {remove,army,munich},
      {disband,army,munich},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_move,army,munich,fleet,kiel,berlin,any_coast},
      {support_hold,fleet,finland,any_unit,brest},
      {support_hold,fleet,finland,any_unit,brest},
      {hold,any_unit,holland},
      {hold,army,brest},
      {hold,army,brest},
      {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,finland,army,brest,marseilles},
      {move,army,brest,marseilles,any_coast},
      {convoy,fleet,gulf_of_lyon,army,brest,marseilles},
      {convoy,fleet,western_mediterranean,army,brest,marseilles},
      {convoy,fleet,finland,army,brest,marseilles},
      {move,army,brest,marseilles,any_coast},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {convoy,fleet,north_sea,army,london,norwegian_sea},
      {move,army,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,north_coast},
      {move,any_unit,london,norwegian_sea,any_coast},
      {move,army,london,norwegian_sea,any_coast}],
     [{error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",["A","Bre","->","F",[],[],[]]}},
      {error,{"invalid action#",["A","Bre","-","F",[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}},
      {error,{"invalid action#",[[],[],"-",[],[],[],[]]}}]}}},
    [
        ?_assertEqual(Expected, ActualOutput)
    ].
