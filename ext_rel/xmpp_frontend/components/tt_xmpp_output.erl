%%%===================================================================
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Jan Daniel Bothma <jbothma@gmail.com>
%%%
%%% @doc Callback module for controller_app to pass responses to
%%% requests via the xmpp_frontend.
%%%
%%% @see controller_app
%%%
%%% @since : 26 Oct 2011 by Bermuda Triangle
%%%==================================================================
-module(tt_xmpp_output).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("command_parser/include/command_parser.hrl").


% controller callback
-export([reply/3]).

-define(SUPPORTED_COMMANDS,
        "REGISTER, LOGIN, UPDATE, CREATE, OVERVIEW").


%%-------------------------------------------------------------------
%% @doc
%% This function replies to the user depending on the result of the
%% request.
%% @end
%%
%% [@spec reply([From, To], {Cmd, Result}, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
reply([From, To], {register, success}, User) ->
    send_chat(From, To,
              "Registration was successful.~n~p~n",
              [User]);
reply([From, To], {register, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid registration data.~n~p~n",
              [Info]);

reply([From, To], {login, success}, Session) ->
    send_chat(From, To,
              "Login was successful. Your session is: \"~p\"~n",
              [Session]);
reply([From, To], {login, invalid_data}, _Info) ->
    send_chat(From, To,
              "Invalid login data.~n");

reply([From, To], {update_user, success}, User) ->
    send_chat(From, To,
              "User information was successfully updated.~n~p~n",
              [User]);
reply([From, To], {update_user, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid user update information.~n~p~n",
              [Info]);

reply([From, To], {create_game, success}, GameID) ->
    send_chat(From, To,
              "Game creation was successful. Your game ID is: \"~p\"~n",
              [GameID]);
reply([From, To], {create_game, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid game creation data.~n~p~n",
              [Info]);

reply([From, To], {reconfig_game, success}, Game) ->
    send_chat(From, To,
              "Game information was successfully updated.~n~p~n",
              [Game]);
reply([From, To], {reconfig_game, invalid_data}, Info) ->
    send_chat(From, To,
              "Invalid reconfig game information.~n~p~n",
              [Info]);

reply([From, To], {join_game, success}, Info) ->
    send_chat(From, To,
              "Join game was successful.~n~p~n", [Info]);
reply([From, To], {join_game, invalid_data}, Error) ->
    send_chat(From, To,
              "Invalid join game data.~n~p~n", [Error]);

reply([From, To], {game_overview, success}, GOV) ->
    Info = lists:flatten(game_overview(GOV)),
    send_message(From, To,
                 "chat", "\nGame Overview:\n" ++ Info);
reply([From, To], {game_overview, invalid_data},
      user_not_playing_this_game) ->
    send_chat(From, To,
              "You are not playing this game");

reply([From, To], {Cmd, invalid_session}, Info) ->
    send_chat(From, To,
              "[~p]Invalid user session.~n~p~n",
              [Cmd, Info]);

reply([From, To], {Cmd, parse_error}, Error) ->
    send_chat(From, To,
              "The command [~p] could not be interpreted correctly:~n~s~n",
              [Cmd, parse_error_msg(Error)]);

reply([From, To], unknown_command, _Data) ->
    send_chat(From, To,
              "The provided command is unknown.~n"
              "Supported commands are:~n"
              ?SUPPORTED_COMMANDS).


%%===================================================================
%% Internal Functions
%%===================================================================
parse_error_msg({error,{required_fields,FieldStrList}}) ->
    io_lib:format("Required fields: ~p", [FieldStrList]);
parse_error_msg({error,{invalid_input,FieldStrList}}) ->
    io_lib:format("Invalid input to the following: ~p", [FieldStrList]).


%%-------------------------------------------------------------------
%% @ doc
%%  send the message to a user as a chat message
%%-------------------------------------------------------------------
send_chat(From, To, Format) ->
    send_chat(From, To, Format, []).

send_chat(From, To, Format, Data) ->
    Body = io_lib:format(Format, Data),
    send_message(From, To, "chat", Body).


%%-------------------------------------------------------------------
%% @ doc
%%  prepare the xml chat mesage and route it to the user
%%-------------------------------------------------------------------
send_message(From, To, TypeStr, BodyStr) ->
    ?INFO_MSG("Sending message type [~s]~n"
              "From ~p~n"
              "To ~p~n"
              "Body \"~s\".~n",
              [TypeStr, From#jid.user, To#jid.user, BodyStr]),
    XmlBody = {xmlelement, "message",
               [{"type", TypeStr},
                {"from", jlib:jid_to_string(From)},
                {"to", jlib:jid_to_string(To)}],
               [{xmlelement, "body", [],
                 [{xmlcdata, strip_bom(BodyStr)}]}]},
    ejabberd_router:route(From, To, XmlBody).


%%-------------------------------------------------------------------
%% @ doc
%%  strip the BOM or Byte Order Mark from the beginning of the body
%%-------------------------------------------------------------------
strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

%%-------------------------------------------------------------------
%% @doc
%% provide game infomation for user which extract from
%%  game record, country and current map of the game
%%  input is the game_overview record and it returns information in string
%% @end
%%-------------------------------------------------------------------
game_overview(#game_overview{} = GOV)->
    Msg1 = lists:flatten(
             io_lib:format("~nYou are playing as ~p ~nGame Information:~n",
                                       [GOV#game_overview.country])),
    Msg2 = get_game_info(GOV#game_overview.game_rec, Msg1),
    io:format("in game_overview after msg: ~p~n ", [Msg2]),
    get_map_info(digraph_io:from_erlang_term(GOV#game_overview.map),
                 Msg2, GOV#game_overview.country).

%%-------------------------------------------------------------------
%% @doc
%% get game record and previous message then return previous message
%%  together with game info
%% @end
%%-------------------------------------------------------------------
get_game_info(#game{} = Game, Msg) ->
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
%% get map as digraph, previous message and country of player
%%    Return previous message together with map info
%% @end
%%-------------------------------------------------------------------
get_map_info(Map, Msg, Country) ->
    MyProvincesAtom = get_my_provinces(Map, Country),
    MyProvinces = lists:foldl(
                    fun (Prov, Acc) ->
                             case map:get_province_info(Map,Prov, center) of
                                 true ->
                                     Str = io_lib:format("~p*~n",
                                                         [atom_to_list(Prov)]),
                                     Acc ++ Str;
                                 undefined ->
                                     Str = io_lib:format("~p~n",
                                                          [atom_to_list(Prov)]),
                                     Acc ++ Str
                             end
                    end, [], MyProvincesAtom),
    Msg1 = Msg ++ "\nYour provinces:\n" ++ MyProvinces,
    Msg1 ++ "\nAll units:\n" ++ get_unit_prov(Map).

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
                                         Str = io_lib:format("~p*: ~p, ~p~n",
                                                       [atom_to_list(Prov),
                                                        atom_to_list(UnitType),
                                                        atom_to_list(Country)]),
                                         Acc ++ Str;
                                     undefined ->
                                         Str = io_lib:format("~p: ~p, ~p~n",
                                                       [atom_to_list(Prov),
                                                        atom_to_list(UnitType),
                                                        atom_to_list(Country)]),
                                         Acc ++ Str
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
            Val = lists:flatten(io_lib:format("~p: ~p ~n", [Label, Value])),
            Msg ++ Val
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
    Msg ++ integer_to_list(Value) ++ Label;
print_time(Msg, _Value, _Label) ->
    Msg.

