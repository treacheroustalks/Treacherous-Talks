-module (map_data).

-export ([create/1,
          delete/1]).

-type map () :: digraph ().

%% -----------------------------------------------------------------------------
%% @doc
%% create a new map with all countries and units and stuff.
%% receives the game mode as parameter, currently, it only supports 
%% standard_game and empty
%% -----------------------------------------------------------------------------
-spec create (GameType) -> Map when
      GameType :: empty | standard_game,
      Map :: map ().
create (empty) ->
    digraph:new ([cyclic, protected]);
create (standard_game) ->
    Map = create (empty),
                                                % initialize provinces:
    [map:add_province (Map, Prov) || Prov <- [% Austria:
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
    [map:connect_provinces (Map, A, B, Types) || 
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
    [map:set_province_info (Map, Id, center, true) || Id <- [vienna, 
                                                                   budapest,
                                                                   trieste,
                                                                   roma,
                                                                   napoli,
                                                                   venezia,
                                                                   berlin,
                                                                   kiel,
                                                                   munich]],
                                                % add the units:
    [map:add_unit (Map, Unit, To) || {To, Unit} <- 
                                               [{vienna, {army, austria}},
                                                {budapest, {army, austria}},
                                                {trieste, {fleet, austria}},
                                                {munich, {army, germany}},
                                                {berlin, {army, germany}},
                                                {kiel, {fleet, germany}},
                                                {roma, {army, italy}},
                                                {napoli, {fleet, italy}},
                                                {venezia, {army, italy}}]],
    Map.

%% -----------------------------------------------------------------------------
%% @doc
%% Delete a map
%% @end
%% -----------------------------------------------------------------------------
-spec delete (map ()) -> ok.
delete (Map) ->
    true = digraph:delete (Map),
    ok.
