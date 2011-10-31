%% -----------------------------------------------------------------------------
%% @author <stephan.brandauer@gmail.com>

%% -----------------------------------------------------------------------------
-module (map_test).

-include_lib ("eunit/include/eunit.hrl").

disable_logger_test () ->
    error_logger:tty (false).

empty_map_setup () ->
    map_data:create (empty).

test_map_setup () ->
    map_data:create (standard_game).

map_teardown (Map) ->
    map_data:delete (Map).

add_province_test () ->
    Map = empty_map_setup (),
    map:add_province (Map, munich),
    ?assertEqual ([munich], digraph:vertices (Map)),
    map_teardown (Map).

get_reachable_test () ->
    Map = test_map_setup (),
    ?assert (set_equiv ([galicia,
                         bohemia,
                         budapest,
                         trieste,
                         tyrolia,
                         vienna],
                        map:get_reachable (Map, vienna, army))),
    ?assertEqual ([vienna], map:get_reachable (Map, vienna, fleet)),
    ?assert (set_equiv ([kiel,
                         ruhr,
                         munich,
                         berlin,
                         prussia,
                         silesia,
                         bohemia,
                         tyrolia],
                        map:get_reachable (Map, kiel, army, 2))),
    ?assert (set_equiv ([helgoland,
                         baltic_sea, 
                         berlin,
                         prussia,
                         kiel],
                        map:get_reachable (Map, kiel, fleet, 2))),
    map_teardown (Map).

vienna_is_center_test () ->
    Map = test_map_setup (),
    ?assert (map:get_province_info (Map, vienna, center)),
    map_teardown (Map).

set_get_province_info_test () ->
    Map = map_data:create (empty),
    map:add_province (Map, first_province),
    ?assertEqual (undefined, 
                  map:get_province_info (Map, first_province, is_center)),
    map:set_province_info (Map, first_province, is_center, true),
    ?assertEqual (true, 
                  map:get_province_info (Map, first_province, is_center)),
    map_data:delete (Map).


%% -----------------------------------------------------------------------------
%% stuff dealing with units
%% -----------------------------------------------------------------------------
add_move_remove_test () ->
    Map = test_map_setup (),
    %% move (A) Vie to Gal
    ?assertEqual ([{army, austria}], map:get_units (Map, vienna)),
    ?assertEqual ([], map:get_units (Map, galicia)),
    map:move_unit (Map, {army, austria}, vienna, galicia),
    ?assertEqual ([], map:get_units (Map, vienna)),
    ?assertEqual ([{army, austria}], map:get_units (Map, galicia)),
    %% now try to do that again! has to fail:
    ?assertEqual (unit_does_not_exist,
                  map:move_unit (Map, {army, austria},
                                 vienna,
                                 galicia)),
    %% now try to move galicia to budapest - two units will stand there, since
    %% budapest has an army already
    ?assertEqual ([{army, austria}], map:get_units (Map, galicia)),
    ?assertEqual ([{army, austria}], map:get_units (Map, budapest)),
    map:move_unit (Map, {army, austria}, galicia, budapest),
    ?assertEqual ([], map:get_units (Map, galicia)),
    ?assertEqual ([{army, austria}, {army, austria}],
                  map:get_units (Map, budapest)),
    map_teardown (Map).

get_provinces_test () ->
    Map = empty_map_setup (),
    map:add_province (Map, "stringonia"),
    map:add_province (Map, {tuple, town}),
    map:add_province (Map, atomic_wasteland),
    ?assert (set_equiv (["stringonia", {tuple, town}, atomic_wasteland], 
                      map:get_provinces (Map))),
    map_teardown (Map).

unit_info_test () ->
    Map = test_map_setup (),
    % make (A) Vie unique:
    Ref = make_ref (),
    map:set_unit_info (Map, {army, austria}, vienna, ref, Ref),
    ?assertEqual (Ref, 
                  map:get_unit_info (Map, {army, austria}, vienna, ref)),
    map:move_unit (Map, {army, austria}, vienna, galicia),
    ?assertEqual (Ref,
                  map:get_unit_info (Map, {army, austria}, galicia, ref)),
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
                           map:get_units (DeserializedMap, vienna)),
                        ?assert (
                           set_equiv (
                             [bohemia, galicia, tyrolia, 
                              trieste, budapest, vienna], 
                             map:get_reachable (Map, vienna, army))),
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
