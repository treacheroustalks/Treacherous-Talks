%% -----------------------------------------------------------------------------
%% @doc
%% rules implements the game rules as in the manual.
%% It uses {@link map} as representation of the board.
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (rules).

-export ([process/3]).

-include_lib ("eunit/include/eunit.hrl").

-type nation () :: russia
                   | turkey
                   | austria
                   | italy
                   | germany
                   | france
                   | england.

-type province () :: atom ().

-type unit_type () :: army | fleet.

-type unit () :: {unit_type (), nation ()}.

-type phase () :: order_phase | retreat_phase | build_phase.

-type year () :: reference ().

-type game_time () :: {phase (), year ()}.

-spec new_time (phase ()) -> game_time ().
new_time (Phase) ->
    {Phase, make_ref ()}.
    
% time = {season (), year ()}, order = order received then.
% stores the last order, a unit received.
-record (history, {time = {undefined, undefined}, order=undefined}).

%% e.g.:
%% {move, {army, austria}, vienna, galicia}
%% {support, {army, austria}, {move, {army, germany}, munich, silesia}}
-type hold_order () :: {hold, unit ()}.
-type move_order () :: {move, unit (), province (), province ()}.
-type support_order () :: {support, unit (), province (), order ()}.
-type convoy_order () :: {convoy, unit (), province (), province ()}.

-type order () :: hold_order () |
                  move_order () |
                  support_order () |
                  convoy_order ().

-type order_return () :: ok | unit_does_not_exist.

-type order_status () :: {order_return () | order ()}.

-type map () :: digraph ().

%% -----------------------------------------------------------------------------
%% @doc
%% processes a list of orders according to the rules and returns the
%% updated map.
%% @end
%% -----------------------------------------------------------------------------
-spec process (phase (), map (), [order ()]) -> [order_status ()].
process (Phase, Map, Orders) ->
    GameTime = new_time (Phase),
    lists:map (fun (O) ->
                       do_process (Phase, GameTime, Map, O)
               end,
               Orders).

-spec do_process (phase (), game_time (), map (), order ()) -> 
                         ok | no_return ().
do_process (_Phase, _GameTime, _Map, {hold, _}) ->
    ok;
do_process (_Phase,
            GameTime,
            Map,
            Order = {move, Unit, From, To}) ->
    case is_legal (Map, Order) of
        true ->
            case unit_can_act (Map, Unit, From, GameTime) of
                true ->
                    % remeber when and where the unit came from:
                    map:set_unit_info (Map, Unit, From, 
                                       history, 
                                       #history{time = GameTime, 
                                                order = Order}),
                    % ..and move it:
                    Status = map:move_unit (Map, Unit, From, To),
                    {Status, Order};
                false ->
                    {unit_does_not_exist, Order}
                end;
        false ->
            {illegal_order, Order}
    end;
do_process (Phase, GameTime, Map, Order) ->
    is_legal (Map, Order),
    erlang:error ({error,
                   {unhandled_case, {?MODULE, ?LINE},
                    [Phase, GameTime, Map, Order]}}).

-spec unit_can_act (map (), unit (), province (), game_time ()) -> boolean ().
unit_can_act (Map, Unit, Id, Time) ->
    case map:get_unit_info (Map, Unit, Id, history) of
        History=#history{time=Time} ->
            % has done something already!
            ?debugVal (History),
            false;
        _Other ->
            true
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% checks, whether an order is valid
%% does ONLY check the map, does not consider whether units actually exist!
%% @end
%% -----------------------------------------------------------------------------
-spec is_legal (map (), order ()) -> boolean ().
is_legal (Map, Order) ->
    case Order of
        {hold, _} ->
            true;
        {move, {Type, _Nation}, From, To} ->
            lists:member (To, 
                          map:get_reachable (Map, From, Type));
        {support, _Unit, _Province, Order} ->
            %% todo: check, if it is neighboring 
            %% (case Order of {move..} | {hold..}
            is_legal (Map, Order);
        {convoy, _Unit, _From, _To} ->
            %% a convoy order is always legal if seen alone.
            %% (Think of chained convoys, they only work when a full ensemble 
            %% of convoy orders fit together.
%            map:unit_exists (Map, Unit) and 
%                is_province (Map, From) and
%                is_province (Map, To).
            true
    end.

-spec replace_element ([any ()], any (), any ()) -> [any ()].
replace_element (List, OldElt, NewElt) ->
    lists:map (fun (Elt) ->
                       case Elt of
                           OldElt -> NewElt;
                           _ -> Elt
                       end
               end,
               List).
%% -----------------------------------------------------------------------------
%% TESTS
%%
%% the tests for non-exported functions are down here. If you believe, that
%% tests and implementation should be strictly seperated: please stop scrolling
%% -----------------------------------------------------------------------------

replace_element_test () ->
    ?assertEqual ([1,2,3,asdf,5],
                  replace_element (lists:seq (1,5), 4, asdf)).

do_process_move_test () ->
    ok.

legal_orders () ->
    [{move, {army, austria}, vienna, galicia},
     {move, {fleet, germany}, berlin, prussia},
     {move, {fleet, germany}, kiel, berlin},
     {move, {army, italy}, roma, apulia}].

illegal_orders () ->
    [{move, {army, austria}, vienna, munich}, % too far!
     {move, {army, germany}, kiel, helgoland},% can't swim!
     {move, {fleet, germany}, kiel, ruhr},    % can't walk!
     {move, {fleet, italy}, roma, apulia}].   % not on same coast!

legal_move_test () ->
    Map = map_data:create (standard_game),
    %% legal orders:
    [?assertEqual (true, is_legal (Map, Order)) || Order <- legal_orders ()],
    %% illegal orders:
    [?assertEqual (false, is_legal (Map, Order)) || Order <- illegal_orders ()],
    map_data:delete (Map).

unit_can_act_test () ->
    io:format(user, "unit_can_act_test", []),
    Map = map_data:create (standard_game),
    Time = new_time (order_phase),
    Unit = {army, austria},
    Order = {move, Unit, vienna, galicia},
    ?debugVal (Time),
    io:format(user, "asserting that unit is able to move..", []),
    ?assertEqual (true, unit_can_act (Map, Unit, vienna, Time)),
    io:format(user, "faking a move this round..", []),
    map:set_unit_info (Map, Unit,
                       vienna, 
                       history, #history{time = Time, 
                                         order = Order}),
    io:format(user, "asserting that unit is not able to move any more..", []),
    ?assertEqual (false, unit_can_act (Map, Unit, vienna, Time)),
    NextTime = {order_phase, make_ref ()},
    io:format(user, "starting next round..", []),
    ?debugVal (NextTime),
    io:format(user, "asserting that unit is able to move again..", []),
    ?assertEqual (true, unit_can_act (Map, Unit, galicia, NextTime)),
    map_data:delete (Map),
    io:format(user, "unit_can_act_test: done", []).
