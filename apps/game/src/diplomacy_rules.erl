%% -----------------------------------------------------------------------------
%% @doc
%% The rules are defined here, the rule engine merely executes them.
%% The rules have an arity that says how many orders are involved in a rule's
%% handling.
%%
%% Example #1: unit_exists: arity=1 because I can decide for every order on
%% its own, whether it complies with the unit_exists-rule. It does not need
%% to be compared.
%%
%% Example #2: trade_places_rule: arity=2 because I have to compare 2 orders to
%% decide, whether they are a violation or not.
%% It needs to be compared with every other Order.
%%
%% Based on the arity, the rule engine will use the rule to
%% compare every possible combination of arity-tuples of Orders.
%%
%% The detector function receives a arity-tuple of orders and has to return
%% true, if a violation of the rule is found, false otherwise.
%% If a violation is found, the actor-function will be called with the offending
%% tuple in the arguments. The actor function's job is to resolve the violation.
%% It can signal changes to the rule engine, by emitting a list of
%% {order, Order}-tuples. As for now, only {remove, Order} is valid as an order
%% but this is easily extended. Look at {@link delete_orders_actor}
%% to see an example.
%%
%% @end
%% -----------------------------------------------------------------------------
-module (diplomacy_rules).

-export ([create/1,
          do_process/3]).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("game/include/rule.hrl").

%-type province () :: atom ().
%-type unit_type () :: army | fleet.

%-type unit () :: {unit_type (), nation ()}.

%-type phase () :: order_phase | retreat_phase | build_phase.

%-type year () :: reference ().

%-type game_time () :: {phase (), year ()}.

%% e.g.:
%% {move, {army, austria}, vienna, galicia}
%% {support, {army, austria}, {move, {army, germany}, munich, silesia}}
%% {convoy, {fleet, england}, english_channel, {army, england}, wales, picardy}

%-type hold_order () :: {hold, unit ()}.
%-type move_order () :: {move, unit (), province (), province ()}.
%-type support_order () :: {support, unit (), province (), order ()}.
%-type convoy_order () :: {convoy, unit (), province (), unit (),
%                          province (), province ()}.
%-type order () :: hold_order () |
%                  move_order () |
%                  support_order () |
%                  convoy_order ().



%-type nation () :: russia
%                   | turkey
%                   | austria
%                   | italy
%                   | germany
%                   | france
%                   | england.

%% -----------------------------------------------------------------------------
%% @doc
%% creates the rules for a game type.
%% The parameter decides the game type,
%% currently, the only supported game type is the atom <pre>standard_game</pre>
%% @end
%% -----------------------------------------------------------------------------
-spec create (GameType) -> [tuple ()] when
      GameType :: standard_game.
create (standard_game) ->
    [unit_exists_rule (),
     unit_cannot_go_there_rule (),
     support_rule (),
     trade_places_rule (),
     bounce2_rule (),
     hold_vs_move2_rule ()].

do_process (_Phase, Map, Order = {move, Unit, From, To}) ->
    ?debugMsg (io_lib:format ("processing ~p~n", [Order])),
    map:move_unit (Map, Unit, From, To);
do_process (_Phase, _Map, _Order) ->
    ok.

unit_exists_rule () ->
    #rule{name = unit_exists,
          arity = 1,
          detector = fun unit_does_not_exist/2,
          actor = fun delete_orders_actor/2}.

trade_places_rule () ->
    #rule{name = trade_places,
          arity = 2,
          detector = fun trade_places_detector/2,
          actor = fun replace_by_hold_actor/2}.

unit_cannot_go_there_rule () ->
    #rule{name = unit_cannot_go_there,
          arity = 1,
          detector = fun unit_cannot_go_there_detector/2,
          actor = fun replace_by_hold_actor/2}.

bounce2_rule () ->
    #rule{name = bounce2,
          arity = 2,
          detector = fun bounce2_detector/2,
          actor = fun bounce2_actor/2}.

hold_vs_move2_rule () ->
    #rule{name = hold_vs_move2,
          arity = 2,
          detector = fun hold_vs_move2_detector/2,
          actor = fun hold_vs_move2_actor/2}.

support_rule () ->
    #rule{name = support_rule,
          arity = 2,
          detector = fun support_detector/2,
          actor = fun support_actor/2}.

%% {support, {army, austria}, {move, {army, germany}, munich, silesia}}
%% {convoy, {fleet, england}, english_channel, {army, england}, wales, picardy}
support_detector (_Map, {_Order, {support, _Unit, _Where, _Order}}) ->
    true;
support_detector (_Map, {{support, _Unit, _Where, _Order}, _Order}) ->
    true;
support_detector (_Map, _Orders = {_, _}) ->
    false.

get_first_unit ({move, Unit, _, _}) ->
    Unit;
get_first_unit ({hold, Unit, _}) ->
    Unit;
get_first_unit ({support, Unit, _Where, _Order}) ->
    Unit;
get_first_unit ({convoy, Fleet, _Where, _Unit, _From, _To}) ->
    Fleet.

get_first_from ({move, _, From, _}) ->
    From;
get_first_from ({hold, _, Where}) ->
    Where;
get_first_from ({support, _Unit, Where, _Order}) ->
    Where;
get_first_from ({convoy, _Fleet, Where, _Unit, _From, _To}) ->
    Where.

%% gets the 'moving' unit, this also means a unit that holds.
%% the distinction is important for support- and convoy-orders
get_moving_unit ({move, Unit, _, _}) ->
    Unit;
get_moving_unit ({hold, Unit, _}) ->
    Unit;
get_moving_unit ({support, _Unit, Order}) ->
    get_moving_unit (Order);
get_moving_unit ({convoy, _Fleet, _Where, Unit, _From, _To}) ->
    Unit.

get_moving_from ({move, _, From, _}) ->
    From;
get_moving_from ({hold, _, Where}) ->
    Where.

get_moving_to ({move, _, _, To}) ->
    To;
get_moving_to ({hold, _, Where}) ->
    Where.

is_supportable ({move, _, _, _}) ->
    true;
is_supportable ({hold, _, _}) ->
    true;
is_supportable (_) ->
    false.


do_add_support (Map,
                SupportingUnit, SupportingFrom,
                SupportedUnit, SupportedWhere) ->
    SupportingUnits =
        map:get_unit_info (Map,
                           SupportedUnit, SupportedWhere, supporting, []),
    case lists:member ({SupportingUnit, SupportingFrom}, SupportingUnits) of
        true ->
            ok;
        false ->
            map:set_unit_info (Map, SupportedUnit, SupportedWhere, supporting,
                               [{SupportingUnit, SupportingFrom} | SupportingUnits])
    end.

support_actor (Map, {OtherOrder, SupOrder = {support, _Unit, _Where, _Order}}) ->
    support_actor (Map, {SupOrder, OtherOrder});
support_actor (Map,
               {SupOrder = {support, SupUnit = {SupType, _}, SupPlace, _Order},
                OtherOrder}) ->
    case (is_supportable (OtherOrder)) of
        true ->
            To = get_moving_to (OtherOrder),
            case map:is_reachable (Map, SupPlace, To, SupType) of
                true ->
                    From = get_moving_from (OtherOrder),
                    do_add_support (Map,
                                    SupUnit, SupPlace,
                                    get_moving_unit (OtherOrder), From),
                    [];
                false ->
                    [{remove, SupOrder}]
            end;
        false ->
            [{remove, SupOrder}]
    end.

%% -----------------------------------------------------------------------------
%% Implementation
%% -----------------------------------------------------------------------------

hold_vs_move2_detector (_Map, {{move, _Unit1, _From1, To},
                               {hold, _Unit2, To}}) ->
    true;
hold_vs_move2_detector (_Map, {{hold, _Unit1, To},
                               {move, _Unit2, _From1, To}}) ->
    true;
hold_vs_move2_detector (_Map, _) ->
    false.

hold_vs_move2_actor (_Map, {{hold, _Unit1, _HoldPlace},
                            Move = {move, Unit2, From, _HoldPlace}}) ->
    [{remove, Move}, {add, {hold, Unit2, From}}].

bounce2_detector (_Map, {{move, _Unit1, _From1, _To},
                         {move, _Unit2, _From2, _To}}) ->
    true;
bounce2_detector (_Map, {_, _}) ->
    false.

get_unit_strength (Map, Unit1, From1) ->
    length (map:get_unit_info (Map, Unit1, From1, supporting, [])).

bounce2_actor (Map,
               {O1 = {move, Unit1, From1, To},
                O2 = {move, Unit2, From2, To}}) ->
    io:format (user, "bounce2 ~p~n", [['map', O1, O2]]),
    Strength1 = get_unit_strength (Map, Unit1, From1),
    Strength2 = get_unit_strength (Map, Unit2, From2),
    if
        Strength1 > Strength2 ->
            [{remove, O2}];
        Strength2 > Strength1 ->
            [{remove, O1}];
        true ->
            [{remove, O1}, {remove, O2}]
    end.

unit_cannot_go_there_detector (Map, {{move, {Type, _Owner}, From, To}}) ->
    not lists:member (To,
                      map:get_reachable (Map, From, Type));
unit_cannot_go_there_detector (_Map, _) ->
    false.

unit_does_not_exist (Map, {Order}) ->
    not map:unit_exists (Map, get_first_from (Order), get_first_unit (Order)).

trade_places_detector (_Map, {{move, _Unit1, From, To},
                              {move, _Unit2, To, From}}) ->
    true;
trade_places_detector (_Map, _Orders) ->
    false.

delete_orders_actor (_Map, {A}) ->
    [{remove, A}];
delete_orders_actor (_Map, {A,B}) ->
    [{remove, A}, {remove, B}];
delete_orders_actor (_Map, {A,B,C}) ->
    [{remove, A}, {remove, B}, {remove, C}].

replace_by_hold_actor (_Map, {A}) ->
    [{remove, A},
     {add, {hold, get_first_unit (A), get_first_from (A)}}];
replace_by_hold_actor (_Map, {A,B}) ->
    [{remove, A}, {remove, B},
     {add, {hold, get_first_unit (A), get_first_from (A)}},
     {add, {hold, get_first_unit (B), get_first_from (B)}}];
replace_by_hold_actor (_Map, {A,B,C}) ->
    [{remove, A}, {remove, B}, {remove, C},
     {add, {hold, get_first_unit (A), get_first_from (A)}},
     {add, {hold, get_first_unit (B), get_first_from (B)}},
     {add, {hold, get_first_unit (C), get_first_from (C)}}].
