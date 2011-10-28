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

-type order_return () :: executed | unit_does_not_exist.

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
    lists:map (fun (O) ->
                       do_process (Phase, Map, O)
               end,
               Orders).

-spec do_process (phase (), map (), order ()) -> ok | no_return ().
do_process (_Phase, _Map, {hold, _}) ->
    ok;
do_process (_Phase,
            Map,
            Order={move, Unit, From, To}) ->
    case is_legal (Map, Order) of
        true ->
            case map:unit_exists (Map, From, Unit) of
                true ->
                    map:move_unit (Map, Unit, From, To),
                    {executed, Order};
                false ->
                    {unit_does_not_exist, Order}
                end;
        false ->
            {illegal_order, Order}
    end;
do_process (Phase, Map, Order) ->
    is_legal (Map, Order),
    erlang:error ({error,
                   {unhandled_case, {?MODULE, ?LINE},
                    [Phase,Map,Order]}}).


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
    ?debugMsg ("legal_move_test"),
    Map = map_data:create (standard_game),
    %% legal orders:
    [?assertEqual (true, is_legal (Map, Order)) || Order <- legal_orders ()],
    %% illegal orders:
    [?assertEqual (false, is_legal (Map, Order)) || Order <- illegal_orders ()],
    ?debugMsg ("legal_move_test: done"),
    map_data:delete (Map).
