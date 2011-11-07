%%%===================================================================
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sbhat7@gmail.com>
%%%
%%% @doc Module to format output suitable for the user
%%%
%%% @since : 07 Nov 2011 by Bermuda Triangle
%%%==================================================================

-module(data_format).

-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("utils/include/command_parser.hrl").

-export([
         game_overview_to_text/1,
         game_to_text/1,
         map_to_text/2,
         gamerec_to_proplist/1, gamerec_to_proplist/2,
         userrec_to_proplist/1, userrec_to_proplist/2
         ]).

%%-------------------------------------------------------------------
%% @doc
%% Convert game overview record into text format
%% @end
%%-------------------------------------------------------------------
game_overview_to_text(#game_overview{} = GOV)->
    Country = GOV#game_overview.country,
    Game = game_to_text(GOV#game_overview.game_rec),
    Map = digraph_io:from_erlang_term(GOV#game_overview.map),
    {Provinces, Units} = map_to_text(Map, Country),
    {Country, Game, Provinces, Units}.


%%-------------------------------------------------------------------
%% @doc
%% Convert game record into text format
%% @end
%%-------------------------------------------------------------------
game_to_text(#game{} = Game) ->
    Msg1 = update_msg("", Game#game.name, ?GAMENAME, undefined),
    Msg2 = update_msg(Msg1, Game#game.description, ?DESCRIPTION, field_missing),
    Msg3 = update_msg(Msg2, Game#game.status, ?STATUS, undefined),
    Msg4 = update_msg(Msg3, Game#game.press, ?PRESSTYPE, undefined),
    Msg5 = update_msg(Msg4, Game#game.order_phase, ?ORDERCIRCLE, undefined, min),
    Msg6 = update_msg(Msg5, Game#game.retreat_phase, ?RETREATCIRCLE, undefined, min),
    Msg7 = update_msg(Msg6, Game#game.build_phase, ?GAINLOSTCIRCLE, undefined, min),
    Msg8 = update_msg(Msg7, Game#game.result, ?RESULT, none),
    Msg9 = update_msg(Msg8, Game#game.waiting_time, ?WAITTIME, undefined, min),
    update_msg(Msg9, Game#game.num_players, ?NUMBEROFPLAYERS, field_missing).


%%-------------------------------------------------------------------
%% @doc
%% Convert digraph map for given country into text format
%% @end
%%-------------------------------------------------------------------
map_to_text(Map, Country) ->
    MyProvincesAtom = get_my_provinces(Map, Country),
    MyProvinces = lists:foldl(
                    fun (Prov, Acc) ->
                             case map:get_province_info(Map,Prov, center) of
                                 true ->
                                     Str = io_lib:format("~s*~n",
                                                         [atom_to_list(Prov)]),
                                     string:concat(Acc, Str);
                                 undefined ->
                                     Str = io_lib:format("~s~n",
                                                          [atom_to_list(Prov)]),
                                     string:concat(Acc, Str)
                             end
                    end, [], MyProvincesAtom),
    {MyProvinces, get_unit_prov(Map)}.


%%-------------------------------------------------------------------
%% @doc
%% get map and coutry then return the country's provinces
%%  Note: Map :: digraph()
%% @end
%%-------------------------------------------------------------------
get_my_provinces(Map, Country) ->
    Provinces= map:get_provinces(Map),
    lists:filter(fun (Prov) ->
                          case map:get_province_info(Map,Prov,owner) of
                              Country ->
                                  true;
                              _->
                                  false
                          end
                 end, Provinces).


%%-------------------------------------------------------------------
%% @doc
%% get map then return all units in the map
%%  Note: Map :: digraph()
%% @end
%%-------------------------------------------------------------------
get_unit_prov(Map) ->
    Provinces= map:get_provinces(Map),
    lists:foldl(
      fun (Prov, Info) ->
               case map:get_units(Map,Prov) of
                   [] ->
                       Info;
                   Units ->
                       lists:foldl(
                         fun({UnitType, Country},Acc )->
                                 case map:get_province_info(Map,Prov, center) of
                                     true ->
                                         Str = io_lib:format("~s*: ~s, ~s~n",
                                                       [atom_to_list(Prov),
                                                        atom_to_list(UnitType),
                                                        atom_to_list(Country)]),
                                         string:concat(Acc, Str);
                                     undefined ->
                                         Str = io_lib:format("~s: ~s, ~s~n",
                                                       [atom_to_list(Prov),
                                                        atom_to_list(UnitType),
                                                        atom_to_list(Country)]),
                                         string:concat(Acc, Str)
                                 end
                         end, Info, Units)
               end
      end, [], Provinces).


%%-------------------------------------------------------------------
%% @doc
%% get message and update the message based on default and current value
%%  input :
%%       Msg, previous message
%%       Value, current value
%%       Label, label
%%       Default: default value if match current value, return previous message
%% @end
%%-------------------------------------------------------------------
update_msg(Msg, Value, Label, Default) ->
    case Value of
        Default ->
            Msg;
        _ ->
            Val = io_lib:format("~s: ~s ~n", [Label, Value]),
            string:concat(Msg, Val)
    end.

update_msg(Msg, Value, Label, Default, min) when is_integer(Value) ->
    D = Value div 1440,
    M1 = print_time("", D, "D"),
    Rem = Value rem 1440,

    H = Rem div 60,
    M2 = print_time(M1, H, "H"),
    M = Rem rem 60,
    M3 = print_time(M2, M, "M"),
    update_msg(Msg, M3, Label, Default);
update_msg(Msg, Value, Label, Default, min) ->
    update_msg(Msg, Value, Label, Default).

print_time(Msg, Value, Label) when Value > 0 ->
    string:concat(string:concat(Msg, integer_to_list(Value)), Label);
print_time(Msg, _Value, _Label) ->
    Msg.


%%-------------------------------------------------------------------
%% @doc
%% Converts game record to proplist
%% input :
%%     Game - record
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
gamerec_to_proplist(Game) ->
    RecordInfo = [atom_to_list(Field) || Field <- record_info(fields, game)],
    lists:zip(RecordInfo, tl(tuple_to_list(Game))).

%%-------------------------------------------------------------------
%% @doc
%% Converts game record to proplist
%% input :
%%     Game - record
%%     Format - string - converts all atom "values" to string format
%%     RequiredFields - used to avoid sending all the data to the user. Only
%%                      fields mentioned in it will be returned
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
gamerec_to_proplist(Game, {Format, RequiredFields}) ->
    PropList = gamerec_to_proplist(Game),
    rec_filter(PropList, {Format, RequiredFields}).


%%-------------------------------------------------------------------
%% @doc
%% Converts user record to proplist
%% input :
%%     User - record
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
userrec_to_proplist(User) ->
    RecordInfo = [atom_to_list(Field) || Field <- record_info(fields, user)],
    lists:zip(RecordInfo, tl(tuple_to_list(User))).

%%-------------------------------------------------------------------
%% @doc
%% Converts user record to proplist
%% input :
%%     User - record
%%     Format - string - converts all atom "values" to string format
%%     RequiredFields - used to avoid sending all the data to the user. Only
%%                      fields mentioned in it will be returned
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
userrec_to_proplist(User, {Format, RequiredFields}) ->
    PropList = userrec_to_proplist(User),
    rec_filter(PropList, {Format, RequiredFields}).


%% Filter records based on format
rec_filter(PropList, {Format, RequiredFields}) ->
    FilterPL = [{X, Y} || {X, Y} <- PropList, lists:member(X, RequiredFields)],
    case Format of
        % All the elements and values are in string format
        string ->
            lists:map(fun({X, Y}) -> {X, case is_atom(Y) of
                                             true -> atom_to_list(Y);
                                             false -> Y
                                         end} end, FilterPL);
        % Default proplist
        _ -> FilterPL
    end.
