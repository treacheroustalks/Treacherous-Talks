%% -----------------------------------------------------------------------------
%% @doc
%% map_utils contains the representation of a map, including where units stand.
%%
%% The module covers only what a real game-board would do, it knows nothing
%% about the rules.
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (map_utils).

-export ([create_map/1,
          delete/1,
          add_province/2,
          connect_provinces/2,
          set_province_type/3,
          get_province_type/2,
          add_unit/3,
          remove_unit/3,
          move_unit/4,
          get_units/2,
          is_center/2,
          set_is_center/3,
          get_reachable/2,
          get_reachable/3,
          create_map/1,
          test_map/0]).

-type prov_id () :: atom ().
-type prov_type () :: land | sea | coastal.

-type unit () :: {army | fleet, Owner :: atom ()}.

-type map () :: digraph ().
-record (province_info, {type :: prov_type (),
                         owner :: any (),
                         is_center :: boolean (),
                         units = [] :: [unit ()] | []}).

%% -----------------------------------------------------------------------------
%% @doc
%% Delete a map
%% @end
%% -----------------------------------------------------------------------------
-spec delete (map ()) -> ok.
delete (Map) ->
    true = digraph:delete (Map),
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% Add a province to a Map.
%% @end
%% -----------------------------------------------------------------------------
-spec add_province (map (), prov_id ()) -> ok.
add_province (Map, Id) ->
    digraph:add_vertex (Map, Id, #province_info{}),
    ok.

-spec is_center (Map, Id) -> boolean () when
      Map :: map (),
      Id :: prov_id ().
is_center (Map, Id) ->
    get_province_info (Map, Id, #province_info.is_center).

-spec set_is_center (Map, Id, Val) -> ok when
      Map :: map (),
      Id :: prov_id (),
      Val :: boolean ().
set_is_center (Map, Id, Val) ->
    set_province_info (Map, Id, #province_info.is_center, Val).

%% -----------------------------------------------------------------------------
%% @doc
%% Set the type of a Province.
%% @end
%% -----------------------------------------------------------------------------
-spec set_province_type (Map, Id, Type) -> ok when
      Map :: map (),
      Id :: prov_id (),
      Type :: prov_type ().
set_province_type (Map, Id, Type) ->
    set_province_info (Map, Id, #province_info.type, Type).

%% -----------------------------------------------------------------------------
%% @doc
%% Get the type of a Province.
%% @end
%% -----------------------------------------------------------------------------
-spec get_province_type (Map, Id) -> Type when
      Map :: map (),
      Id :: prov_id (),
      Type :: prov_type ().
get_province_type (Map, Id) ->
    get_province_info (Map, Id, #province_info.type).

-spec set_province_info (map (), prov_id (), pos_integer (), any ()) -> ok.
set_province_info (Map, Id, Field, NewValue) ->
    {Id, P=#province_info{}} = digraph:vertex (Map, Id),
    digraph:add_vertex (Map, Id, setelement (Field, P, NewValue)),
    ok.

-spec get_province_info (map (), prov_id (), pos_integer ()) -> any ().
get_province_info (Map, Id, Field) ->
    {Id, P=#province_info{}} = digraph:vertex (Map, Id),
    element (Field, P).

-spec get_reachable (Map, Id) -> [prov_id ()] when
      Map :: map (),
      Id :: prov_id ().
get_reachable (Map, Id) ->
    ordsets:from_list (
      digraph:out_neighbours (Map, Id)).

-spec add_unit (Map :: map (), Unit :: unit (), To :: prov_id ()) -> ok.
add_unit (Map, Unit, To) ->
    Units = get_province_info (Map, To, #province_info.units),
    set_province_info (Map, To, #province_info.units, [Unit | Units]).

-spec remove_unit (Map :: map (), Unit :: unit (), From :: prov_id ()) ->
                          ok | no_return ().
remove_unit (Map, Unit, From) ->
    Units = get_province_info (Map, From, #province_info.units),
    case lists:member (Unit, Units) of
        false ->
            erlang:error ({error,
                           {remove_unit, [Map, Unit, From], unit_not_there}});
        true ->
            set_province_info (Map, From,
                               #province_info.units,
                               lists:delete (Unit, Units))
    end.

-spec move_unit (Map :: map (), Unit :: unit (), From :: prov_id (), To :: prov_id ()) ->
                        ok | no_return ().
move_unit (Map, Unit, From, To) ->
    remove_unit (Map, Unit, From),
    add_unit (Map, Unit, To).

get_units (Map, Id) ->
    get_province_info (Map, Id, #province_info.units).

-spec get_reachable (Map, prov_id (), Degree) -> [prov_id ()] when
      Map :: map (),
      Degree :: pos_integer ().
get_reachable (Map, Id, Degree)  when Degree > 1 ->
    DirectNeighbours = get_reachable (Map, Id),
    ordsets:union ([DirectNeighbours | [get_reachable (Map, Neigh, Degree-1)
                                      || Neigh <- DirectNeighbours]]);
get_reachable (Map, Id, 1) ->
    get_reachable (Map, Id).

%% -----------------------------------------------------------------------------
%% @doc
%% Create a connection (two-way) between two provinces
%% @end
%% -----------------------------------------------------------------------------
connect_provinces (Map, {Id1, Id2}) ->
    digraph:add_edge (Map, Id1, Id2, {Id1, Id2}),
    digraph:add_edge (Map, Id2, Id1, {Id2, Id1}),
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% create a new map with all countries and units and stuff.
%% receives the game mode as parameter, currently, it only supports 
%% standard_game and empty
%% -----------------------------------------------------------------------------
create_map (empty) ->
    digraph:new ([cyclic, protected]);
create_map (standard_game) ->
    Map = create_map (empty),
    % initialize provinces:
    [add_province (Map, Prov) || Prov <- [bohemia,
                                          galicia,
                                          vienna,
                                          budapest,
                                          trieste,
                                          tyrolia,
                                          munich,
                                          silesia,
                                          prussia,
                                          berlin,
                                          kiel,
                                          ruhr,
                                          helgoland,
                                          baltic_sea]],
    % set province terrain types:
    [set_province_type (Map, Id, Type) || {Type, Id} <-
                                              [{land, bohemia},
                                               {land, galicia},
                                               {land, vienna},
                                               {land, budapest},
                                               {coastal, trieste},
                                               {land, tyrolia},
                                               {land, munich},
                                               {land, silesia},
                                               {coastal, prussia},
                                               {coastal, berlin},
                                               {coastal, kiel},
                                               {land, ruhr},
                                               {sea, helgoland},
                                               {sea, baltic_sea}]],
    % connect neighbouring provinces:
    [connect_provinces (Map, Pair) || Pair <- [{bohemia, galicia},
                                               {bohemia, vienna},
                                               {bohemia, tyrolia},
                                               {bohemia, silesia},
                                               {bohemia, munich},
                                               {tyrolia, trieste},
                                               {tyrolia, vienna},
                                               {tyrolia, munich},
                                               {vienna, trieste},
                                               {vienna, budapest},
                                               {vienna, galicia},
                                               {munich, ruhr},
                                               {munich, berlin},
                                               {munich, silesia},
                                               {munich, kiel},
                                               {ruhr, kiel},
                                               {kiel, helgoland},
                                               {kiel, berlin},
                                               {kiel, baltic_sea},
                                               {berlin, baltic_sea},
                                               {berlin, silesia},
                                               {berlin, prussia},
                                               {prussia, baltic_sea},
                                               {prussia, silesia},
                                               {helgoland, baltic_sea}]],
    % add the units:
    [add_unit (Map, Unit, To) || {To, Unit} <- [{vienna, {army, austria}},
                                                {budapest, {army, austria}},
                                                {trieste, {fleet, austria}},
                                                {munich, {army, germany}},
                                                {berlin, {army, germany}},
                                                {kiel, {army, germany}}]],
    Map.

%% -----------------------------------------------------------------------------
%% @doc
%% return Austria's and Germany's start position
%% @end
%% -----------------------------------------------------------------------------
-spec test_map () -> map ().
test_map () ->
    create_map (standard_game).
