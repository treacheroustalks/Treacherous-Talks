%% -----------------------------------------------------------------------------
%% @doc
%% rules implements the game rules as in the manual.
%% It uses {@link map_utils} as representation of the board.
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (rules).

-export ([process/3]).

-include_lib ("eunit/include/eunit.hrl").

-type province () :: atom ().

-type unit_type () :: army | fleet.

-type unit () :: {unit_type (), province ()}.

-type phase () :: order_phase | retreat_phase | build_phase.

%% e.g.:
%% {move, {army, munich}, tyrolia}
%% {support, {army, bohemia}, {move, {army, munich}, tyrolia}}
-type hold_order () :: {hold, unit ()}.
-type move_order () :: {move, unit (), province ()}.
-type support_order () :: {support, unit (), order ()}.
-type convoy_order () :: {convoy, unit (), province (), province ()}.

-type order () :: hold_order () |
                  move_order () |
                  support_order () |
                  convoy_order ().

-type map () :: digraph ().

%% -----------------------------------------------------------------------------
%% @doc
%% processes a list of orders according to the rules and returns the
%% updated map.
%% @end
%% -----------------------------------------------------------------------------
-spec process (phase (), map (), [order ()]) -> map ().
process (Phase, Map, [Order | Orders]) ->
    process (Phase,
             do_process (Phase, Map, Order),
             Orders);
process (_Phase, Map, []) ->
    Map.

-spec do_process (phase (), map (), order ()) -> map () | none ().
do_process (_Phase, Map, {hold, _}) ->
    Map;
do_process (_Phase,
            Map,
            Order={move, _Unit, _Province}) ->
    case is_legal (Map,Order) of
        true ->
            Map
    end;
do_process (Phase, Map, Order) ->
    is_legal (Map, Order),
    erlang:error ({error,
                   {unhandled_case, {?MODULE, ?LINE},
                    [Phase,Map,Order]}}).

%% -----------------------------------------------------------------------------
%% @doc
%% checks, whether an order is valid
%% @todo: check for unit-existence
%% @end
%% -----------------------------------------------------------------------------
-spec is_legal (map (), order ()) -> boolean ().
is_legal (Map, Order) ->
    case Order of
        {hold, _} ->
            true;
        {move, {UType, UPlace}, Province} ->
            lists:member (Province, 
                          map_utils:get_reachable (Map, UPlace, UType));
        {support, _Unit, Order} ->
            is_legal (Map, Order);
        {convoy, _Unit, _From, _To} ->
            %% a convoy order is always legal if seen alone.
            %% (Think of chained convoys, they only work when a full ensemble 
            %% of convoy orders fit together.
%            map_utils:unit_exists (Map, Unit) and 
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

legal_move_test () ->
    ?debugMsg ("legal_move_test"),
    Map = map_utils:create_map (standard_game),
    %% legal orders:
    ?assertEqual (true, is_legal (Map, {move, {army, vienna}, galicia})),
    ?assertEqual (true, is_legal (Map, {move, {fleet, helgoland}, baltic_sea})),
    ?assertEqual (true, is_legal (Map, {move, {fleet, baltic_sea}, prussia})),
    ?assertEqual (true, is_legal (Map, {move, {fleet, berlin}, kiel})),
    ?assertEqual (true, is_legal (Map, {move, {army, roma}, apulia})),
    %% illegal orders:
    % too far:
    ?assertEqual (false, is_legal (Map, {move, {army, vienna}, munich})),
    % can't swim:
    ?assertEqual (false, is_legal (Map, {move, {army, kiel}, helgoland})),
    % can't walk:
    ?assertEqual (false, is_legal (Map, {move, {fleet, kiel}, ruhr})),
    % not on same coast:
    ?assertEqual (false, is_legal (Map, {move, {fleet, roma}, apulia})),
    ?debugMsg ("legal_move_test: done"),
    map_utils:delete (Map).
