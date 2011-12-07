%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
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
-include_lib("datatypes/include/message.hrl").
-include_lib("utils/include/command_parser.hrl").

-export([
         game_overview_to_text/1,
         games_list_to_text/1,
         game_to_text/1,
         map_to_text/2,
         rec_to_plist/1, rec_to_plist/2, plist_to_rec/2,
         type_of/1,
         search_result_keys/1,
         bin_to_int/1,
         date_to_str/1,
         user_to_text/1,
         db_obj_to_rec/2
         ]).

%%-------------------------------------------------------------------
%% @doc
%% Convert db object to record
%% @spec  db_obj_to_rec(#db_obj{}, string()) ->
%%    #user{}| #game{}
%% @end
%%-------------------------------------------------------------------
db_obj_to_rec(DbObj, RecordName) ->
    PropList = db_obj:get_value(DbObj),
    data_format:plist_to_rec(RecordName, PropList).

%%-------------------------------------------------------------------
%% @doc
%% Convert game overview record into text format
%% @end
%%-------------------------------------------------------------------
game_overview_to_text(#game_overview{} = GOV)->
    GameStatus = GOV#game_overview.game_rec#game.status,
    case GameStatus of
        finished -> create_game_info(finished, GOV);
        Status -> create_game_info(Status, GOV)
    end.


%%-------------------------------------------------------------------
%% @doc
%% Produces the game overview data, based on the status of the game
%% @end
%%-------------------------------------------------------------------
create_game_info(finished, GOV) ->
    GameRec = GOV#game_overview.game_rec,
    GameInfo = game_info_to_text(GOV),
    PlayerInfo = player_list_to_text(GOV#game_overview.players),
    Game = game_to_text(GameRec),
    Map = digraph_io:from_erlang_term(GOV#game_overview.map),
    FinalMap = get_unit_prov(Map),
    {GameRec#game.id, finished,
     {GameInfo, PlayerInfo, Game, FinalMap}};

create_game_info(Status, GOV) ->
    GameRec = GOV#game_overview.game_rec,
    Game = game_to_text(GameRec),
    GameInfo = game_info_to_text(GOV),
    Orders = GOV#game_overview.order_list,
    Map = digraph_io:from_erlang_term(GOV#game_overview.map),
    Country = GOV#game_overview.country,
    {Provinces, Units} = map_to_text(Map, Country),
    {GameRec#game.id, Status,
     {GameInfo, Country, Game, Provinces, Units, Orders}}.


%%-------------------------------------------------------------------
%% @doc
%% Creates a textual list of player, country pairs
%% @end
%%-------------------------------------------------------------------
player_list_to_text(PlayerList) ->
    lists:foldl(
      fun({Country, Nick}, Acc) ->
              String = io_lib:format("~s - ~s~n",
                                     [atom_to_list(Country),
                                      Nick]),
              string:concat(Acc, String) end,
      [], PlayerList).


%%-------------------------------------------------------------------
%% @doc
%% Creates basic game information, in textual form.
%% @end
%%-------------------------------------------------------------------
game_info_to_text(#game_overview{} = GOV) ->
    {Year, Season} = GOV#game_overview.year_season,
    Phase = GOV#game_overview.phase,
    io_lib:format("Phase: ~s~nYear: ~s~nSeason: ~s~n~n",
                  [atom_to_list(Phase),
                   integer_to_list(Year),
                   atom_to_list(Season)
                  ]).

%%-------------------------------------------------------------------
%% @doc
%% Convert list of games to textual form
%% @end
%%-------------------------------------------------------------------
-spec games_list_to_text([#game{}]) -> string().
games_list_to_text(Games) ->
     textual_form_of(Games).

%%-------------------------------------------------------------------
%% Helper function for games_list_to_text(Games)
%% Calls game_to_text for all games in the Games list
%%-------------------------------------------------------------------
textual_form_of([H | T]) ->
    Games = game_to_text(H)
            ++ "\n--------------------\n\n"
            ++ textual_form_of(T),
    [Games];
textual_form_of([]) -> [].

%%------------------------------------------------------------------------------
%% @doc
%%   extract bacis information from user record and covert it to string
%% @end
%%------------------------------------------------------------------------------
-spec user_to_text(#user{}) -> string().
user_to_text(User = #user{}) ->
    Msg = "Your Profile:~n",
    Msg1  = update_msg(Msg, User#user.nick, ?NICKNAME, undefined),
    Msg2  = update_msg(Msg1, User#user.name, ?FULLNAME, undefined),
    Msg3  = update_msg(Msg2, User#user.email, ?EMAIL, undefined),
    Msg4  = update_msg(Msg3, User#user.role, ?ROLE, undefined),
    Msg5  = update_msg(Msg4, User#user.channel, ?CHANNEL, undefined),
    update_msg(Msg5, date_to_str(User#user.date_created), ?DATE_CREATED, undefined).

%%-------------------------------------------------------------------
%% @doc date_to_str/1
%%
%% convert erlang date to string
%% @end
%% [@spec get( date ) @end]
%%-------------------------------------------------------------------
-spec date_to_str(date() ) -> string().
date_to_str(Date) ->
    {{Year, Month, Day}, {H, M, S}} = calendar:universal_time_to_local_time(Date),
    io_lib:format("<~p/~p/~p ~p:~p:~p> ", [Year, Month, Day, H, M, S]).

%%-------------------------------------------------------------------
%% @doc
%% Convert game record into text format
%% @end
%%-------------------------------------------------------------------
game_to_text(#game{} = Game) ->
    Msg  = update_msg("", Game#game.id, ?GAMEID, undefined),
    Msg1 = update_msg(Msg, Game#game.name, ?GAMENAME, undefined),
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
            NewValue = case type_of(Value) of
                           integer -> integer_to_list(Value);
                           float -> float_to_list(Value);
                           tuple -> tuple_to_list(Value);
                           binary -> binary_to_list(Value);
                           atom -> atom_to_list(Value);
                           _ -> Value
                       end,
            Val = io_lib:format("~s: ~s ~n", [Label, NewValue]),
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
%% Converts record to proplist
%% Any new data type has to be added here before using this function
%% because record_info requires that the record name is known at compile time.
%% WARNING: Nested records are not converted
%% input :
%%     RecordValue - record
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
rec_to_plist(RecordValue) ->
    Record = get_record(RecordValue),
    RecFields = case Record of
                    game -> record_info(fields, game);
                    user -> record_info(fields, user);
                    message -> record_info(fields, message);
                    game_message -> record_info(fields, game_message)
                end,
    RecordInfo = [Field || Field <- RecFields],
    lists:zip(RecordInfo, tl(tuple_to_list(RecordValue))).

%%-------------------------------------------------------------------
%% @doc
%% Converts record to proplist
%% input :
%%     RecordValue - record
%%     Format - string - converts all atom "values" to string format
%%     RequiredFields - used to avoid sending all the data to the user. Only
%%                      fields mentioned in it will be returned
%% output:
%%     Proplist - [{Key, Value}]
%% @end
%%-------------------------------------------------------------------
rec_to_plist(RecordValue, {Format, RequiredFields}) ->
    FilterPL = rec_to_plist(RecordValue, RequiredFields),
    case Format of
        string ->
            % All the elements and values to string format
            lists:map(fun({X, Y}) -> {atom_to_list(X),
                                      case is_atom(Y) of
                                          true -> atom_to_list(Y);
                                          false -> Y
                                      end} end, FilterPL);
        % Default proplist
        _ -> FilterPL
    end;
rec_to_plist(RecordValue, RequiredFields) ->
    PropList = rec_to_plist(RecordValue),
    [{X, Y} || {X, Y} <- PropList, lists:member(X, RequiredFields)].


%%-------------------------------------------------------------------
%% @doc
%% Converts proplist to record
%% input :
%%     Record - atom for the record
%%     PropList - proplist
%% output:
%%     Record
%% @end
%%-------------------------------------------------------------------
plist_to_rec(Record, PropList) ->
    Values = plist_get_all_values(PropList),
    list_to_tuple([Record|Values]).

%% ------------------------------------------------------------------
%% @doc
%%  Riak search results tend to be in format [[Bucket, KeyBin],...].
%%  This turns that into [KeyInt,...]
%% @end
%% ------------------------------------------------------------------
-spec search_result_keys(list(list(binary()))) -> list(integer()).
search_result_keys(Result) ->
    lists:map(fun([_,KeyBin]) -> bin_to_int(KeyBin) end,
              Result).

% Hack to get record type from record value
get_record(RecordValue) ->
    element(1, RecordValue).

% Get all values from a proplist
plist_get_all_values(PropList) ->
    lists:reverse(plist_get_all_values(PropList, [])).

plist_get_all_values([], Acc) ->
    Acc;
plist_get_all_values([{_Key, Value}|Ps], Acc) ->
    plist_get_all_values(Ps, [Value|Acc]).

%%-------------------------------------------------------------------
%% @doc
%% Get the type of the variable
%% @end
%%-------------------------------------------------------------------
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X)                     -> unknown.

bin_to_int(B) ->
    list_to_integer(binary_to_list(B)).
