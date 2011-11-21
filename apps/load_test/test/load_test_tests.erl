-module(load_test_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("utils/include/player_orders.hrl").


order_transform_test() ->
    Move = {move,{army,england},yorkshire,edinburgh},
    MoveExp = #move{subj_unit = army, subj_src_loc = yorkshire,
                    subj_dst_loc = edinburgh},
    MoveRes = load_test:order_transform(Move),
    ?assertEqual(MoveExp, MoveRes),

    Hold = {hold, {army, england}, yorkshire},
    HoldExp = #hold{subj_unit = army, subj_loc = yorkshire},
    HoldRes = load_test:order_transform(Hold),
    ?assertEqual(HoldExp, HoldRes),

    SupMove = {support, {army, england}, yorkshire,
               {move,{army,austria},tyrolia,trieste}},
    SupMoveExp = #support_move{subj_unit = army, subj_loc = yorkshire,
                               obj_unit = army, obj_src_loc = tyrolia,
                               obj_dst_loc = trieste},
    SupMoveRes = load_test:order_transform(SupMove),
    ?assertEqual(SupMoveExp, SupMoveRes),

    SupHold = {support, {army, england}, yorkshire,
               {hold, {army, england}, yorkshire}},
    SupHoldExp = #support_hold{subj_unit = army, subj_loc = yorkshire,
                               obj_unit = army, obj_loc = yorkshire},
    SupHoldRes = load_test:order_transform(SupHold),
    ?assertEqual(SupHoldExp, SupHoldRes),

    Convoy = {convoy, {fleet, england}, yorkshire,
              {army, england}, london, paris},
    ConvoyExp = #convoy{subj_unit = fleet, subj_loc = yorkshire,
                        obj_unit = army, obj_src_loc = london,
                        obj_dst_loc = paris},
    ConvoyRes = load_test:order_transform(Convoy),
    ?assertEqual(ConvoyExp, ConvoyRes),

    Build = {build,{army,italy},rome},
    BuildExp = #build{obj_unit = army, obj_loc = rome},
    BuildRes = load_test:order_transform(Build),
    ?assertEqual(BuildExp, BuildRes),

    Destroy = {destroy, {army, england}, paris},
    DestroyExp = #disband{subj_unit = army, subj_loc = paris},
    DestroyRes = load_test:order_transform(Destroy),
    ?assertEqual(DestroyExp, DestroyRes).
    
