%% -----------------------------------------------------------------------------
%% @author <stephan.brandauer@gmail.com>

%% -----------------------------------------------------------------------------
-module (map_utils_test).

-include_lib ("eunit/include/eunit.hrl").

disable_logger_test () ->
    error_logger:tty (false).

empty_map_setup () ->
    map_utils:create_map (empty).

test_map_setup () ->
    map_utils:create_map (standard_game).

map_teardown (Map) ->
    map_utils:delete (Map).

add_province_test () ->
    Map = empty_map_setup (),
    map_utils:add_province (Map, munich),
    ?assertEqual ([munich], digraph:vertices (Map)),
    map_teardown (Map).

set_get_province_type_test () ->
    Map = empty_map_setup (),
    map_utils:add_province (Map, center_munich),
    map_utils:set_province_type (Map, center_munich, center),
    ?assertEqual (center, map_utils:get_province_type (Map, center_munich)),
    map_teardown (Map).

get_reachable_test () ->
    Map = test_map_setup (),
    ?assert (set_equiv ([galicia,
                         bohemia,
                         budapest,
                         trieste,
                         tyrolia],
                        map_utils:get_reachable (Map, vienna))),
%    ?debugVal (map_utils:get_reachable (Map, kiel, 2)),
    ?assert (set_equiv ([kiel,
                         ruhr,
                         munich,
                         berlin,
                         prussia,
                         silesia,
                         bohemia,
                         tyrolia,
                         helgoland,
                         baltic_sea],
                        map_utils:get_reachable (Map, kiel, 2))),
    map_teardown (Map).

%% -----------------------------------------------------------------------------
%% stuff dealing with units
%% -----------------------------------------------------------------------------
add_move_remove_test () ->
    Map = test_map_setup (),
    %% move (A) Vie to Gal
    ?assertEqual ([{army, austria}], map_utils:get_units (Map, vienna)),
    ?assertEqual ([], map_utils:get_units (Map, galicia)),
    map_utils:move_unit (Map, {army, austria}, vienna, galicia),
    ?assertEqual ([], map_utils:get_units (Map, vienna)),
    ?assertEqual ([{army, austria}], map_utils:get_units (Map, galicia)),
    %% now try to do that again! has to fail:
    ?assertException (error, _,
                      map_utils:move_unit (Map, {army, austria},
                                           vienna,
                                           galicia)),
    %% now try to move galicia to budapest - two units will stand there, since
    %% budapest has an army already
    ?assertEqual ([{army, austria}], map_utils:get_units (Map, galicia)),
    ?assertEqual ([{army, austria}], map_utils:get_units (Map, budapest)),
    map_utils:move_unit (Map, {army, austria}, galicia, budapest),
    ?assertEqual ([], map_utils:get_units (Map, galicia)),
    ?assertEqual ([{army, austria}, {army, austria}],
                  map_utils:get_units (Map, budapest)),
    map_teardown (Map).

%% -----------------------------------------------------------------------------
%% serialization/deserialization
%% -----------------------------------------------------------------------------
serialization_test () ->
    Map = test_map_setup (),
    SerialMap = digraph_io:to_erlang_term (Map),
    %% do this in a different thread - that's the reason why we need it, after
    %% all:
    Self = self (),
    ?debugVal (Self),
    spawn_link (fun () ->
                        DeserializedMap = 
                            digraph_io:from_erlang_term (SerialMap),
                        ?assertEqual (
                           [{army, austria}], 
                           map_utils:get_units (DeserializedMap, vienna)),
                        ?assert (
                           set_equiv (
                             [bohemia, galicia, tyrolia, trieste, budapest], 
                             map_utils:get_reachable (Map, vienna))),
                        Self ! test_done
                end),
    receive test_done -> ok end,
    map_teardown (Map).

%% -----------------------------------------------------------------------------
%% helpers
%% -----------------------------------------------------------------------------
set_equiv_test () ->
    ?assert (set_equiv ([1,2,3],[2,3,1])).

set_equiv (A, B) ->
    lists:all (fun (EltB) ->
                       lists:member (EltB, A)
               end,
               B)
        and (length (A) == length (B)).
