%% -----------------------------------------------------------------------------
%% @doc
%% map contains the representation of a map, including where units stand.
%%
%% The module covers only what a real game-board would do, it knows nothing
%% about the rules.
%% The module uses dictionaries to store random information for each unit,
%% use the set/get_unit/province_info functions for this task.
%% @author <stephan.brandauer@gmail.com>
%% @end
%% -----------------------------------------------------------------------------
-module (map).

-include_lib ("eunit/include/eunit.hrl").

-export ([add_province/2,
          get_provinces/1,
          connect_provinces/4,
          add_unit/3,
          unit_exists/3,
%          pop_stored_unit/3,
          move_unit/4,
          get_units/2,
          get_province_info/3,
          set_province_info/4,
          get_unit_info/4,
          set_unit_info/5,
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
-record (stored_unit, {unit,info}).

%% -----------------------------------------------------------------------------
%% @doc
%% Add a province to a Map.
%% @end
%% -----------------------------------------------------------------------------
-spec add_province (map (), prov_id ()) -> ok.
add_province (Map, Id) ->
    digraph:add_vertex (Map, Id, create_province_info ()),
    ok.

get_provinces (Map) ->
    digraph:vertices (Map).

create_info () ->
    dict:new ().

create_province_info () ->
    Dict = create_info (),
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
                _Other ->
                    undefined
            end;
        false ->
            not_found
    end.

-spec get_unit_dict (map (), unit (), prov_id ()) -> dict ().
get_unit_dict (Map, Unit, Id) ->
    StoredUnits = get_province_info (Map, Id, units),
    case lists:keyfind (Unit, #stored_unit.unit, StoredUnits) of
        #stored_unit{info = Dict} ->        
            Dict;
        false ->
            create_info ()
    end.

-spec set_unit_dict (map (), unit (), prov_id (), dict ()) -> ok.
set_unit_dict (Map, Unit, Id, Dict) ->
    StoredUnits = get_province_info (Map, Id, units),
    NewStoredUnit = #stored_unit{unit=Unit, info=Dict},
    NewStoredUnits = lists:keyreplace (Unit, #stored_unit.unit, 
                                       StoredUnits, NewStoredUnit),
    set_province_info (Map, Id, units, NewStoredUnits).

-spec get_unit_info (map (), unit (), prov_id (), any ()) -> any ().
get_unit_info (Map, Unit, Id, Key) ->
    Dict = get_unit_dict (Map, Unit, Id),
    case dict:find (Key, Dict) of
        {ok, InfoValue} ->
            InfoValue;
        _Other ->
            undefined
    end.

set_unit_info (Map, Unit, Id, Key, Value) ->
    NewDict = dict:store (Key, Value, get_unit_dict (Map, Unit, Id)),
    set_unit_dict (Map, Unit, Id, NewDict).

%% -----------------------------------------------------------------------------
%% @doc
%% adds a unit to a province without any checking. Assumes that you know where
%% you are placing it
%% @end
%% -----------------------------------------------------------------------------
-spec add_unit (Map :: map (), Unit :: unit (), To :: prov_id ()) -> ok.
add_unit (Map, Unit, To) ->
    add_stored_unit (Map, #stored_unit{unit=Unit, info=create_info ()}, To).

add_stored_unit (Map, Unit=#stored_unit{}, To) ->
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
    lists:keymember (Unit, #stored_unit.unit, Units).

%% -----------------------------------------------------------------------------
%% @doc
%% Removes a unit from a province.
%% Will throw {error, [Map, Unit, From], unit_not_there} if the unit does not
%% exist.
%% @end
%% -----------------------------------------------------------------------------
-spec pop_stored_unit (Map, Unit, From) ->
                              #stored_unit{} | unit_does_not_exist when
      Map :: map (),
      Unit :: unit (),
      From :: prov_id ().
pop_stored_unit (Map, Unit, From) ->
    Units = get_province_info (Map, From, units),
    case lists:keyfind (Unit, #stored_unit.unit, Units) of
        false ->
            unit_does_not_exist;
        StoredUnit ->
            set_province_info (Map, From,
                               units,
                               lists:delete (StoredUnit, Units)),
            StoredUnit
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% Moves a unit from one province to another.
%% exist.
%% @end
%% -----------------------------------------------------------------------------
-spec move_unit (Map, Unit, From, To) -> ok | no_return () when
      Map :: map (),
      Unit :: unit (),
      From :: prov_id (),
      To :: prov_id ().
move_unit (Map, Unit, From, To) ->
    case pop_stored_unit (Map, Unit, From) of
        SUnit = #stored_unit{} ->
            add_stored_unit (Map, SUnit, To);
        Other ->
            Other
    end.

get_units (Map, Id) ->
    lists:map (fun (#stored_unit{unit=Unit}) ->
                       Unit
               end,
               get_province_info (Map, Id, units)).

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
