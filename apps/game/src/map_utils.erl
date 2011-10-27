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

-include_lib ("eunit/include/eunit.hrl").

-export ([create_map/1,
          delete/1,
          add_province/2,
          connect_provinces/4,
          add_unit/3,
          unit_exists/3,
          remove_unit/3,
          move_unit/4,
          get_units/2,
          get_province_info/3,
          set_province_info/4,
          get_reachable/3,
          get_reachable/4]).

-type prov_id () :: any ().

-type unit_type () :: any ().
-type unit () :: {unit_type (), Owner :: any ()}.

-type map () :: digraph ().
%-record (province_info, {owner :: any (),
%                         is_center :: boolean (),
%                         units = [] :: [unit ()] | []}).

-record (connection_info, {types=[]}).

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
    digraph:add_vertex (Map, Id, create_province_info ()),
    ok.

create_province_info () ->
    Dict = dict:new (),
    dict:store (units, [], Dict).

-spec set_province_info (map (), prov_id (), any (), any ()) -> ok.
set_province_info (Map, Id, Key, Value) ->
    {Id, Dict} = digraph:vertex (Map, Id),
    digraph:add_vertex (Map, Id, dict:store (Key, Value, Dict)),
    ok.

-spec get_province_info (map (), prov_id (), any ()) -> any ().
get_province_info (Map, Id, Key) ->
    case digraph:vertex (Map, Id) of
        {Id, Dict} ->
            case dict:find (Key, Dict) of
                {ok, Value} ->
                    Value;
                error ->
                    io:format (user,"UNDEFINED: ~p@~p~n", [Key, Id]),
                    undefined
            end;
        false ->
            not_found
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% adds a unit to a province without any checking. Assumes that you know where
%% you are placing it
%% @end
%% -----------------------------------------------------------------------------
-spec add_unit (Map :: map (), Unit :: unit (), To :: prov_id ()) -> ok.
add_unit (Map, Unit, To) ->
    Units = get_province_info (Map, To, units),
    set_province_info (Map, To, units, [Unit | Units]).

%% -----------------------------------------------------------------------------
%% @doc
%% Checks for a certain unit in a given province
%% @end
%% -----------------------------------------------------------------------------

-spec unit_exists (Map, Id, Unit) -> boolean () when
      Map :: map (),
      Id :: prov_id (),
      Unit :: unit ().
unit_exists (Map, Id, Unit) ->
    Units = get_province_info (Map, Id, units),
    lists:member (Unit, Units).

%% -----------------------------------------------------------------------------
%% @doc
%% Removes a unit from a province.
%% Will throw {error, [Map, Unit, From], unit_not_there} if the unit does not
%% exist.
%% @end
%% -----------------------------------------------------------------------------
-spec remove_unit (Map, Unit, From) -> ok | no_return () when
      Map :: map (),
      Unit :: unit (),
      From :: prov_id ().
remove_unit (Map, Unit, From) ->
    Units = get_province_info (Map, From, units),
    case lists:member (Unit, Units) of
        false ->
            erlang:error ({error,
                           {remove_unit, [Map, Unit, From], unit_not_there}});
        true ->
            set_province_info (Map, From,
                               units,
                               lists:delete (Unit, Units))
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% Moves a unit from one province to another.
%% Equivalent to {@link remove_unit/3} and {@link add_unit/3}.
%% exist.
%% @end
%% -----------------------------------------------------------------------------
-spec move_unit (Map, Unit, From, To) -> ok | no_return () when
      Map :: map (),
      Unit :: unit (),
      From :: prov_id (),
      To :: prov_id ().
move_unit (Map, Unit, From, To) ->
    remove_unit (Map, Unit, From),
    add_unit (Map, Unit, To).

get_units (Map, Id) ->
    get_province_info (Map, Id, units).

-spec get_reachable (Map, From, UnitType) -> [prov_id ()] when
      Map :: map (),
      From :: prov_id (),
      UnitType :: unit_type ().
get_reachable (Map, From, UnitType) ->
    get_reachable (Map, From, UnitType, 1).

%% -----------------------------------------------------------------------------
%% @doc
%% returns every province that is reachable within <code>Degree</code> moves
%% @end
%% -----------------------------------------------------------------------------
-spec get_reachable (Map, From, UnitType, Degree) -> [prov_id ()] when
      Map :: map (),
      From :: prov_id (),
      UnitType :: unit_type (),
      Degree :: pos_integer ().
get_reachable (Map, From, UnitType, Degree)  when Degree > 1 ->
    DirectNeighbours = get_reachable (Map, From, UnitType),
    ordsets:union ([DirectNeighbours | [get_reachable (Map, 
                                                       Neigh, 
                                                       UnitType, 
                                                       Degree-1)
                                        || Neigh <- DirectNeighbours]]);
get_reachable (Map, From, UnitType, 1) ->
    % take those outgoing edges, where UnitType is in #connection_info.types,
    % return an ordered set (because the first clause implicitly expects that 
    % by using ordsets:union)
    ordsets:from_list (
      lists:foldl (fun (E, Acc) ->
                           {_E, From, To, #connection_info{types=Types}} =
                               digraph:edge (Map, E),
                           case lists:member (UnitType, Types) of
                               true ->
                                   [To | Acc];
                               false ->
                                   Acc
                           end
                   end, 
                   [From], 
                   digraph:out_edges (Map, From))).

%% -----------------------------------------------------------------------------
%% @doc
%% Create a connection (two-way) between two provinces
%% @end
%% -----------------------------------------------------------------------------
-spec connect_provinces (Map, Id1, Id2, Types) -> ok when
      Map :: map (),
      Id1 :: prov_id (),
      Id2 :: prov_id (),
      Types :: [unit_type ()].
connect_provinces (Map, Id1, Id2, Types) ->
    digraph:add_edge (Map, Id1, Id2, #connection_info{types=Types}),
    digraph:add_edge (Map, Id2, Id1, #connection_info{types=Types}),
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% create a new map with all countries and units and stuff.
%% receives the game mode as parameter, currently, it only supports 
%% standard_game and empty
%% -----------------------------------------------------------------------------
-spec create_map (GameType) -> Map when
      GameType :: empty | standard_game,
      Map :: map ().
create_map (empty) ->
    digraph:new ([cyclic, protected]);
create_map (standard_game) ->
    Map = create_map (empty),
    % initialize provinces:
    [add_province (Map, Prov) || Prov <- [% Austria:
                                          bohemia,
                                          galicia,
                                          vienna,
                                          budapest,
                                          trieste,
                                          tyrolia,
                                          % Germany:
                                          munich,
                                          silesia,
                                          prussia,
                                          berlin,
                                          kiel,
                                          ruhr,
                                          helgoland,
                                          baltic_sea,
                                          % Italy:
                                          venezia,
                                          piemonte,
                                          tuscany,
                                          roma,
                                          apulia,
                                          napoli,
                                          adriatic_sea,
                                          tyrhennian_sea
                                         ]],
    % connect neighbouring provinces:
    [connect_provinces (Map, A, B, Types) || 
        {A, B, Types} <- [
                          {bohemia, galicia, [army]},
                          {bohemia, vienna, [army]},
                          {bohemia, tyrolia, [army]},
                          {bohemia, silesia, [army]},
                          {bohemia, munich, [army]},
                          {tyrolia, trieste, [army]},
                          {tyrolia, vienna, [army]},
                          {tyrolia, munich, [army]},
                          {tyrolia, venezia, [army]},
                          {trieste, venezia, [army, fleet]},
                          {vienna, trieste, [army]},
                          {vienna, budapest, [army]},
                          {vienna, galicia, [army]},
                          {munich, ruhr, [army]},
                          {munich, berlin, [army]},
                          {munich, silesia, [army]},
                          {munich, kiel, [army]},
                          {ruhr, kiel, [army]},
                          {kiel, helgoland, [fleet]},
                          {kiel, berlin, [army,fleet]},
                          {kiel, baltic_sea, [fleet]},
                          {berlin, baltic_sea, [fleet]},
                          {berlin, silesia, [army]},
                          {berlin, prussia, [army,fleet]},
                          {prussia, baltic_sea, [fleet]},
                          {prussia, silesia, [army]},
                          {helgoland, baltic_sea, [fleet]},
                          {venezia, piemonte, [army]},
                          {venezie, apulia, [army, fleet]},
                          {venezia, tuscany, [army, fleet]},
                          {venezia, roma, [army]},
                          {tuscany, piemonte, [army, fleet]},
                          {tuscany, roma, [army, fleet]},
                          {tuscany, tyrhennian_sea, [fleet]},
                          {roma, apulia, [army]},
                          {roma, napoli, [army, fleet]},
                          {roma, tyrhennian_sea, [fleet]},
                          {napoli, apulia, [army, fleet]},
                          {napoli, tyrhennian_sea, [fleet]}]],
    % set centers:
    [set_province_info (Map, Id, center, true) || Id <- [vienna, 
                                                         budapest,
                                                         trieste,
                                                         roma,
                                                         napoli,
                                                         venezia,
                                                         berlin,
                                                         kiel,
                                                         munich]],
    % add the units:
    [add_unit (Map, Unit, To) || {To, Unit} <- [{vienna, {army, austria}},
                                                {budapest, {army, austria}},
                                                {trieste, {fleet, austria}},
                                                {munich, {army, germany}},
                                                {berlin, {army, germany}},
                                                {kiel, {fleet, germany}},
                                                {roma, {army, italy}},
                                                {napoli, {fleet, italy}},
                                                {venezia, {army, italy}}]],
    Map.

set_get_province_info_test () ->
    Map = create_map (empty),
    add_province (Map, first_province),
    ?assertEqual (undefined, 
                  get_province_info (Map, first_province, is_center)),
    set_province_info (Map, first_province, is_center, true),
    ?assertEqual (true, 
                  get_province_info (Map, first_province, is_center)),
    delete (Map).
