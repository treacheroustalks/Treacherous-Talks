%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc This modules sends replies to the user.
%%%
%%% @end
%%%
%%% @since : 25 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(smtp_output).

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
%% [@spec reply([From, To, ToHost], {Cmd, Result}, Data) -> ok.
%% @end]
%%-------------------------------------------------------------------
%% user registration
reply([From, To, ToHost], {register, success}, User) ->
    send_mail(To, From, ToHost,
              "Registration was successful.~n~p~n",
              [User]);
reply([From, To, ToHost], {register, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid registration data.~n~p~n",
              [Info]);
%% user login
reply([From, To, ToHost], {login, success}, Session) ->
    send_mail(To, From, ToHost,
              "Login was successful. Your session is: \"~p\"~n",
              [Session]);
reply([From, To, ToHost], {login, invalid_data}, _Info) ->
    send_mail(To, From, ToHost,
              "Invalid login data.~n");
%% user update
reply([From, To, ToHost], {update_user, success}, User) ->
    send_mail(To, From, ToHost,
              "User information was successfully updated.~n~p~n",
              [User]);
reply([From, To, ToHost], {update_user, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid user update information.~n~p~n",
              [Info]);
%% game create
reply([From, To, ToHost], {create_game, success}, Game) ->
    send_mail(To, From, ToHost,
              "Game creation was successful. Your game ID is: \"~p\"~n",
              [Game]);
reply([From, To, ToHost], {create_game, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid game creation data.~n~p~n",
              [Info]);
%% game reconfiguration
reply([From, To, ToHost], {reconfig_game, success}, Game) ->
    send_mail(To, From, ToHost,
              "Game information was successfully updated.~n~p~n",
              [Game]);
reply([From, To, ToHost], {reconfig_game, invalid_data}, Info) ->
    send_mail(To, From, ToHost,
              "Invalid reconfig game information.~n~p~n",
              [Info]);
%% game join
reply([From, To, ToHost], {join_game, success}, Info) ->
    send_mail(To, From, ToHost,
              "Join game was successful.~n~p~n",
              [Info]);
reply([From, To, ToHost], {join_game, invalid_data}, Error) ->
    send_mail(To, From, ToHost,
              "Invalid join game data.~n~p~n",
              [Error]);
%% game overview
reply([From, To, ToHost], {game_overview, success}, {ok, GOV}) ->
    Info = lists:flatten(game_overview(GOV)),
    send_mail(To, From, ToHost,
              "\nGame Overview:\n" ++ Info);
reply([From, To, ToHost], {game_overview, invalid_data},
                          user_not_play_this_game) ->
    send_mail(To, From, ToHost,
              "You do not play this game");
%% error
reply([From, To, ToHost], {Cmd, invalid_session}, Info) ->
    send_mail(To, From, ToHost,
              "[~p]Invalid user session.~n~p~n",
              [Cmd, Info]);
reply([From, To, ToHost], {Cmd, parse_error}, Error) ->
    send_mail(To, From, ToHost,
              "The command[~p] could not be interpreted correctly:~n~p~n",
              [Cmd, Error]);
reply([From, To, ToHost], unknown_command, _Data) ->
    send_mail(To, From, ToHost,
              "The provided command is unknown.~n"
              "Supported commands are:~n"
              ?SUPPORTED_COMMANDS).


%% internal function
send_mail(From, To, ToHost, Format) ->
    io:format("[SMTP][To: ~p] ~s", [To, Format]),
    gen_smtp_client:send({From, [To], Format}, [{relay, ToHost}, {port,25}]).
send_mail(From, To, ToHost, Format, Data) ->
    send_mail(From, To, ToHost,
              io_lib:format(Format, Data)).


%%-------------------------------------------------------------------
%% @doc
%% provide game infomation for user which extract from
%%  game record, country and current map of the game
%%  input is the game_overview record and it returns information in string
%% @end
%%-------------------------------------------------------------------
game_overview(#game_overview{} = GOV) ->
    Msg1 = lists:flatten(
                     io_lib:format(
                                "~nYou are playing as ~p ~nGame Information:~n",
                                [GOV#game_overview.country])),
    Msg2 = get_game_info(GOV#game_overview.game_rec, Msg1),
    % io:format("in game_overview after Msg2: ~p~n ", [Msg2]),
    get_map_info(
        digraph_io:from_erlang_term(GOV#game_overview.map),
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
    Msg5 = update_msg(Msg4, Game#game.order_phase, ?ORDERCIRCLE, undefined),
    Msg6 = update_msg(Msg5, Game#game.retreat_phase, ?RETREATCIRCLE, undefined),
    Msg7 = update_msg(Msg6, Game#game.build_phase, ?GAINLOSTCIRCLE, undefined),
    Msg8 = update_msg(Msg7, Game#game.result, ?RESULT, none),
    Msg9 = update_msg(Msg8, Game#game.waiting_time, ?WAITTIME, undefined),
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
                                                        atom_to_list(Country)]
                                                       ),
                                    Acc ++ Str;
                                undefined ->
                                    Str = io_lib:format("~p: ~p, ~p~n",
                                                        [atom_to_list(Prov),
                                                        atom_to_list(UnitType),
                                                        atom_to_list(Country)]
                                                       ),
                                    Acc ++ Str
                            end
                        end, Info, Units)
            end
        end, [], Provinces).

%%-------------------------------------------------------------------
%% @doc
%% get message and update the message based on default and current value
%%  input arguments:
%%       Msg: previous message
%%       Value: current value
%%       Label: label
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
