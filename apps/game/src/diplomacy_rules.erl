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

-export ([create/1]).

-include_lib ("eunit/include/eunit.hrl").
-include_lib ("game/include/rule.hrl").

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
     trade_places_rule (),
     unit_cannot_go_there_rule (),
     bounce2_rule (),
     hold_vs_move2_rule ()].

unit_exists_rule () ->
    #rule{name = unit_exists,
          arity = 1,
          detector = fun unit_does_not_exist/2,
          actor = fun delete_orders_actor/2}.

trade_places_rule () ->
    #rule{name = trade_places,
          arity = 2,
          detector = fun trade_places_detector/2,
          actor = fun delete_orders_actor/2}.

unit_cannot_go_there_rule () ->
    #rule{name = unit_cannot_go_there,
          arity = 1,
          detector = fun unit_cannot_go_there_detector/2,
          actor = fun delete_orders_actor/2}.

bounce2_rule () ->
    #rule{name = bounce2,
          arity = 2,
          detector = fun bounce2_detector/2,
          actor = fun delete_orders_actor/2}.

hold_vs_move2_rule () ->
    #rule{name = hold_vs_move2,
          arity = 2,
          detector = fun hold_vs_move2_detector/2,
          actor = fun hold_vs_move2_actor/2}.

%% -----------------------------------------------------------------------------
%% Implementation
%% -----------------------------------------------------------------------------

%%this is assuming equal strengths:
hold_vs_move2_detector (_Map, {{move, _Unit1, _From1, To},
                               {hold, _Unit2, To}}) ->
    true;
hold_vs_move2_detector (_Map, {{hold, _Unit1, To},
                               {move, _Unit2, _From1, To}}) ->
    true;
hold_vs_move2_detector (_Map, _) ->
    false.

hold_vs_move2_actor (_Map, {{hold, _Unit1, _HoldPlace},
                            Move = {move, _Unit2, _From, _HoldPlace}}) ->
    [{remove, Move}].

bounce2_detector (_Map, {{move, _Unit1, _From1, _To},
                         {move, _Unit2, _From2, _To}}) ->
    true;
bounce2_detector (_Map, {_, _}) ->
    false.

unit_cannot_go_there_detector (Map, {{move, {Type, _Owner}, From, To}}) ->
    not lists:member (To,
                      map:get_reachable (Map, From, Type));
unit_cannot_go_there_detector (_Map, _) ->
    false.

unit_does_not_exist (Map, {{hold, Unit, Where}}) ->
    not map:unit_exists (Map, Where, Unit);
unit_does_not_exist (Map, {{move, Unit, From, _To}}) ->
    not map:unit_exists (Map, From, Unit);
unit_does_not_exist (Map, {{support, Unit, Province, _Order}}) ->
    not map:unit_exists (Map, Province, Unit);
unit_does_not_exist (Map, {{convoy, Fleet, Sea, Army, From, _To}}) ->
    not (map:unit_exists (Map, Sea, Fleet) and
         map:unit_exists (Map, From, Army)).

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

%standoff_detector (Map, Other) ->
%
