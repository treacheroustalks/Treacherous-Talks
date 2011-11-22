%% -----------------------------------------------------------------------------
%% @copyright
%% Copyright (C) 2011 by Bermuda Triangle
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% @end
%% -----------------------------------------------------------------------------
%% @author <stephan.brandauer@gmail.com>
%% @doc
%% generates moves that can be used for load testing.<br/>
%% Warning: a trained monkey would kick this module's ass in the game!
%%
%% an "order-`dict'" is a dict, that contains some standardized fields:
%% <table border="1">
%%   <tr>
%%     <td>Key</td>
%%     <td>Value</td>
%%     <td>Number</td>
%%     <td>Comment</td>
%%   </tr>
%%   <tr>
%%     <td>`phase'</td>
%%     <td>`atom ()'</td>
%%     <td>`1..1'</td>
%%     <td>the game phase the orders are valid for</td>
%%   </tr>
%%   <tr>
%%     <td>`season'</td>
%%     <td>`spring | fall'</td>
%%     <td>`1..1'</td>
%%     <td>the season the orders are valid for</td>
%%   </tr>
%%   <tr>
%%     <td>`year'</td>
%%     <td>integer ()</td>
%%     <td>`1..1'</td>
%%     <td>the year the orders are valid for</td>
%%   </tr>
%%   <tr>
%%     <td>`{orders, Nation}'</td>
%%     <td>`[Orders :: tuple ()]'</td>
%%     <td>`0..inf'</td>
%%     <td>for each nation that has units, there is one such key</td>
%%   </tr>
%% </table>
%% @end
%% -----------------------------------------------------------------------------
-module(gen_moves).
-vsn("1.0.0").

-include_lib ("eunit/include/eunit.hrl").

-export ([generate_orders/1,
          apply_order_season/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% picks a random element from the list
%% @end
%% -----------------------------------------------------------------------------
-spec pick ([any ()] | []) -> any () | {error, empty}.
pick ([]) ->
    {error, empty};
pick (List) ->
    lists:nth (random:uniform (length (List)), List).

%% uncommented to disable the warning:
%-spec weighted_pick ([{pos_integer (), T}]) -> T when
%      T :: any ().
%weighted_pick (WList) ->
%    WSum = lists:foldl (fun ({W, _}, Sum) ->
%                                W + Sum
%                        end, 0, WList),
%    Rand = random:uniform (WSum),
%    SWList = lists:reverse (lists:keysort (1, WList)),
%    {_, Item} =
%        lists:foldl (fun ({W, Item}, {Balance, OldItem}) ->
%                             {Balance - W,
%                              if Balance > 0 ->
%                                      Item;
%                                 true ->
%                                      OldItem
%                              end}
%                     end,
%                     {Rand, none},
%                     SWList),
%    {Rand, Item}.

-spec gen_move (any (), tuple (), atom ()) -> tuple ().
gen_move (Map, Unit = {Type, _}, From) ->
    Options = lists:delete (From, map:get_reachable (Map, From, Type)),
    ?debugVal ({Map, Unit, From, '->', Options}),
    case pick (Options) of
        {error, empty} ->
            {hold, Unit, From};
        To ->
            {move, Unit, From, To}
    end.

-spec gen_build (Map, Nation) -> tuple () when
      Map :: any (),
      Nation :: atom ().
gen_build (Map, Nation) ->
    Provinces = map:get_provinces (Map),
    OriginalCenters =
        lists:filter (
          fun (Province) ->
                  (map:get_province_info (Map,
                                          Province,
                                          original_owner) == Nation)
                      and
                        (map:get_province_info (Map,
                                                Province,
                                                center) == true)
          end, Provinces),
    OriginalCenter = pick (OriginalCenters),
    case map:get_units (Map, OriginalCenter) of
        [] ->
            {build, {pick ([army, fleet]), Nation}, OriginalCenter};
        [Unit | _] ->
            {disband, Unit, OriginalCenter}
    end.

%-spec gen_support (any (), tuple (), atom ()) -> tuple ().
%gen_support (Map, Unit, Where) ->
%    {RndProv, RndUnit} = map:get_units (Map),
%    {support, Unit, Where, gen_move (Map, RndUnit, RndProv)}.

%gen_move_test () ->
%    Map = map_data:create (standard_game),
%    Moves = [gen_move (Map, {army, austria}, vienna) || _X <- lists:seq (1,10)],
%    map_data:delete (Map).

-spec generate_order (Phase :: any (), Map :: any (),
                      Unit :: tuple (), Where :: atom ()) -> tuple ().
generate_order (order_phase, Map, Unit, Where) ->
    gen_move (Map, Unit, Where);
generate_order (build_phase, Map, {_, Nation}, _) ->
    gen_build (Map, Nation);
generate_order (retreat_phase, Map, Unit, Where) ->
    gen_move (Map, Unit, Where);
generate_order (Phase, _, _, _) ->
    erlang:error ({error,
                   {generate_order, ?MODULE, ?LINE, unhandled_phase, Phase}}).

%% return at least 2, not more than 10 years of game play:
-define (MIN_YEARS, 2).
-define (MAX_YEARS, 10).

%% -----------------------------------------------------------------------------
%% @todo add possibility to translate the orders to `lists' so we can include the parser in our load testing, too.
%% @doc
%% Returns a list of  order-`dicts'. <br/>
%% Don't rely on the order of those dicts,
%% use the included `year'/`season'/`phase' infos instead.
%% @end
%% -----------------------------------------------------------------------------
-spec generate_orders (Map) -> [dict ()] when
      Map :: any ().
generate_orders (Map) ->
    PrivateMap = digraph_io:from_erlang_term (digraph_io:to_erlang_term (Map)),
    AliveNations = ordsets:to_list (
                     lists:foldl (
                       fun ({_, {_, Nation}}, Nations) ->
                               ordsets:add_element (Nation, Nations)
                       end,
                       ordsets:new (), map:get_units (PrivateMap))),
    NumberOfYears = ?MIN_YEARS + random:uniform (?MAX_YEARS - ?MIN_YEARS),
    Years = lists:seq (1901, 1901 + NumberOfYears),
    SeasonPhases = [{spring, order_phase},
                    {spring, retreat_phase},
                    {fall, order_phase},
                    {fall, retreat_phase},
                    {fall, build_phase}],
    [create_order_season (PrivateMap, SeasonPhase, Year, AliveNations) ||
        Year <- Years,
        SeasonPhase <- SeasonPhases].

-spec create_order_season (Map, SeasonPhase, Year, AliveNations) -> dict () when
      Map :: any (),
      SeasonPhase :: {atom (), atom ()},
      Year :: integer (),
      AliveNations :: [any ()].
create_order_season (Map, {Season, Phase}, Year, AliveNations) ->
    Dict =
        lists:foldl (fun (Nation, Dict) ->
                             dict_add_order_season (Map, Phase, Nation, Dict)
                     end,
                     dict:new (),
                     AliveNations),
    AugmentedDict =
        lists:foldl (
          fun ({Key, Value}, DictAcc) ->
                  dict:store (Key, Value, DictAcc)
          end,
          Dict,
          [{phase, Phase},
           {season, Season},
           {year, Year}]),
    %% we process the orders, so the next generated season can work with
    %% the results of this one:
    apply_order_season (Map, AugmentedDict),
    AugmentedDict.

-spec dict_add_order_season (Map, Phase, Nation, dict ()) ->
                                    dict () when
      Map :: any (),
      Phase :: atom (),
      Nation :: atom ().
dict_add_order_season (Map, Phase, Nation, Dict) ->
    Units = map:get_units (Map),
    NationUnits = lists:filter (fun ({_, {_, N}}) -> N == Nation end,
                                Units),
    {SomeUnits, _} = lists:split (random:uniform (length (NationUnits)),
                                  NationUnits),
    ?debugVal (SomeUnits),
    Orders =
        lists:foldl (fun ({Where, Unit}, Orders) ->
                             [generate_order (Phase, Map, Unit, Where) | Orders]
                     end,
                     [],
                     SomeUnits),
    dict:store ({orders, Nation}, Orders, Dict).

%% -----------------------------------------------------------------------------
%% @doc
%% receives a map and a single order dict ({@link generate_orders/1} gives you
%% a whole list of them!) and applies the orders to a map.
%% It returns the answer-list from {@link game:process/4}.
%% @end
%% -----------------------------------------------------------------------------
-spec apply_order_season (Map, OrderDict) ->
                                 [Reply] when
      Map :: any (),
      OrderDict :: dict (),
      Reply :: any ().
apply_order_season (Map, OrderDict) ->
    AllOrders = dict:fold (fun (Key, Value, AllOrders) ->
                                   case Key of
                                       {orders, _Nation} ->
                                           AllOrders ++ Value;
                                       _ ->
                                           AllOrders
                                   end
                           end,
                           [], OrderDict),
    Phase = dict:fetch (phase, OrderDict),
    ?debugVal (AllOrders),
    rules:process (Phase, Map, diplomacy_rules, AllOrders).
