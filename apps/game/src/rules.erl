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
-spec process (phase (), map (), [order ()]) -> ok.
process (Phase, Map, [Order | Orders]) ->
    do_process (Phase, Map, Order),
    process (Phase,
             Map,
             Orders);
process (_Phase, Map, []) ->
    ok.

-spec do_process (phase (), map (), order ()) -> ok | no_return ().
do_process (_Phase, Map, {hold, _}) ->
    ok;
do_process (_Phase,
            Map,
            Order={move, _Unit, _Province}) ->
    case is_legal (Map,Order) of
        true ->
            ok
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
%    ?debugVal (Order),
    case Order of
        {hold, _} ->
            true;
        {move, {UType, UPlace}, Province} ->
%            ?debugVal (map_utils:get_reachable (Map, UPlace, UType)),
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

legal_orders () ->
    [{move, {army, vienna}, galicia},
     {move, {fleet, berlin}, prussia},
     {move, {fleet, kiel}, berlin},
     {move, {army, roma}, apulia}].

illegal_orders () ->
    [{move, {army, vienna}, munich}, % too far!
     {move, {army, kiel}, helgoland},% can't swim!
     {move, {fleet, kiel}, ruhr},    % can't walk!
     {move, {fleet, roma}, apulia}]. % not on same coast!

legal_move_test () ->
    ?debugMsg ("legal_move_test"),
    Map = map_utils:create_map (standard_game),
    %% legal orders:
    [?assertEqual (true, is_legal (Map, Order)) || Order <- legal_orders ()],
    %% illegal orders:
    [?assertEqual (false, is_legal (Map, Order)) || Order <- illegal_orders ()],
    ?debugMsg ("legal_move_test: done"),
    map_utils:delete (Map).
