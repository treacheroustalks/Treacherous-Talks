%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @doc player_orders
%%%
%%% A module for recognizing play move orders in email body
%%%
%%% @TODO modify the code in a concurrent a way, so that results of some
%%% expensive initiations can be stored in other processes.
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(player_orders).

%% Exports for API
-export([parse_orders/1]).

%% Exports for eunit
-export([init_valid_region/0,translate_location/1,interpret_str_orders/1,
         translate_abbv_to_fullname_atom/1,interpret_order_line/1]).

-include("test_utils.hrl").
-include("player_orders.hrl").
-include("command_parser.hrl").

%%------------------------------------------------------------------------------
%% @doc parse player's orders to a list of order terms.
%%  Return {ok, {OrderList, ErrorList}}
%%
%%  Example:
%%  Input:  "F boh -> nwy         \n
%%           A vie H              \n
%%           A mun S A bor -> swe "
%%
%%  Output: {ok, {[#move{...},
%%                 #hold{...},
%%                 #support_move{...}],
%%                [{error, ...},
%%                 {error, ...}]}}
%% @end
%%------------------------------------------------------------------------------
parse_orders (EmailBody) when is_binary(EmailBody) ->
    parse_orders (binary_to_list(EmailBody));
parse_orders ([]) -> {error, "empty order"};
parse_orders (EmailBody) ->
    init_valid_region(),
    MailLines = string:tokens(EmailBody, "\n,"),

    % error will be throw out from get_field_value/2
    catch begin
          [RawSessionId|RestLines] = MailLines,
          SessionId = get_field_value(RawSessionId, ?SESSION":\s*([0-9]*)\s*"),

          [RawGameId|RawOrderList] = RestLines,
          GameId = get_field_value(RawGameId, ?GAMEID":\s*([0-9]*)\s*"),

          OrderList = interpret_str_orders(RawOrderList),
          ResultOrders = lists:partition(fun(X)->
                                        element(1, X) /= error end, OrderList),
          % ResultOrders = {[order], [error]}
          case ResultOrders of
              {GameOrderList, []} ->
                  {ok, SessionId, {GameId, GameOrderList}};
              {_, Error} ->
                  {error,{invalid_input, Error}}
          end
    end.

% this function should be only used by parse_orders/1
get_field_value(Data, Pattern) ->
    Match = re:run(Data, Pattern,
                        [{capture, all_but_first, list},{newline, anycrlf}]),
    case Match of
        {match, [Value]} -> Value;
        nomatch -> throw({error, Data ++ "#invalid value#" ++ Pattern})
    end.


%%------------------------------------------------------------------------------
%% @doc interpret each mail line to erlang terms
%%  Example:
%%  Input :["F boh -> nwy         \r",
%%          "A vie H              ",
%%          "A mun S A bor -> swe "]
%%
%%  Output: [#move{...},
%%           #hold{...},
%%           #support_move{...}]
%% @end
%%------------------------------------------------------------------------------
interpret_str_orders (MailLines) ->
    {ok, OrderParser} = re:compile(?ORD_PARSER, [caseless, {newline, anycrlf}]),
    interpret_str_orders(MailLines, OrderParser, []).

interpret_str_orders ([], _, StrOrderList) -> StrOrderList;
interpret_str_orders ([CurrentLine|Rest], OrderParser, StrOrderList) ->
    ExtractResult = re:run(CurrentLine, OrderParser, ?ORD_PARSER_SETTING),
    case ExtractResult of
        {match, ExtractedStrOrderLine} ->
            InterpretedLine = (catch interpret_order_line(ExtractedStrOrderLine)),
            case InterpretedLine of
                {'EXIT', _} ->
                    interpret_str_orders (Rest, OrderParser, StrOrderList);
                _ ->
                    interpret_str_orders (Rest, OrderParser, [InterpretedLine|StrOrderList])
            end;
        nomatch ->
            interpret_str_orders (Rest, OrderParser, StrOrderList)
    end.

%%------------------------------------------------------------------------------
%% @doc interpret a single player order string line to erlang terms
%%  Example:
%%  Input :"F boh -> nwy         \r"
%%
%%  Output: #move{subj_unit=fleet, subj_loc=boh, subj_dst=nwy}
%% @end
%%------------------------------------------------------------------------------
interpret_order_line (OrderLine) ->
    [SubjUnitStr, SubjLocStr, SubjActStr, ObjUnitStr, ObjSrcStr, ObjDstStr,
     CoastStr] = OrderLine,
    SubjAct = translate_action(SubjActStr),
    SubjUnit = translate_unit(SubjUnitStr),
    SubjLoc = translate_location(SubjLocStr),
    ObjUnit = translate_unit(ObjUnitStr),
    ObjSrc = translate_location(ObjSrcStr),
    ObjDst = translate_location(ObjDstStr),
    Coast = translate_coast(CoastStr),

    case SubjAct of
        move when SubjLoc /=nil, ObjSrc/=nil ->
            #move{subj_unit = SubjUnit, subj_src_loc = SubjLoc,
                  subj_dst_loc = ObjSrc, coast = Coast};
        support when ObjDst == nil, SubjLoc /=nil, ObjSrc /=nil ->
            #support_hold{subj_unit = SubjUnit, subj_loc = SubjLoc,
                          obj_unit = ObjUnit, obj_loc = ObjSrc};
        support when SubjLoc /=nil, ObjSrc /=nil, ObjDst /=nil ->
            #support_move{subj_unit = SubjUnit, subj_loc = SubjLoc,
                          obj_unit = ObjUnit, obj_src_loc = ObjSrc,
                          obj_dst_loc = ObjDst, coast = Coast};
        hold when SubjLoc /=nil->
            #hold{subj_unit = SubjUnit, subj_loc = SubjLoc};
        convoy when SubjLoc /=nil, ObjSrc /=nil, ObjDst /=nil ->
            #convoy{subj_unit = SubjUnit, subj_loc = SubjLoc, obj_unit = ObjUnit,
                    obj_src_loc = ObjSrc, obj_dst_loc = ObjDst};
        build when ObjUnit /=nil, ObjSrc /=nil ->
            % @TODO default coast for special coastal province
            #build{obj_unit = ObjUnit, obj_loc = ObjSrc, coast = Coast};
        remove when ObjSrc /= nil ->
            #remove{obj_unit = ObjUnit, obj_loc = ObjSrc};
        disband when SubjLoc /= nil ->
            #disband{subj_unit = SubjUnit, subj_loc = SubjLoc};
        waive ->
            #waive{};
        _ ->
            throw({error, {"invalid action#",OrderLine}})
    end.

% functions prefix with translate_
% should only be called by interpret_order_line/1----------------------------
translate_location([]) -> nil;
translate_location(Loc) when length(Loc) == 3 ->
    translate_abbv_to_fullname_atom(Loc);
translate_location(Loc) ->
    LowercasedLoc = string:to_lower(Loc),
    ExistingAtom = (catch list_to_existing_atom(LowercasedLoc)),
    case ExistingAtom of
        {'EXIT', _} ->
            throw({error, Loc ++ "#invalid location name, not in atom table"});
        MatchedAtom ->
            case get(MatchedAtom) of
                true -> MatchedAtom;
                undefined ->
                    throw({error, Loc ++ "#invalid location name, not in procdict"})
            end
    end.


translate_coast(Key) ->
    get_translation(Key, ?TRANS_COAST, "coast name").


translate_unit(Key) ->
    get_translation(Key, ?TRANS_UNIT, "unit name").


translate_action(Key) ->
    get_translation(Key, ?TRANS_ACTION, "action name").


translate_abbv_to_fullname_atom(Key) ->
    get_translation(Key, ?TRANS_LOC_ABBV, "loc abbv").


get_translation(Key, PropList, ErrorMsg) ->
    LowercasedKey = string:to_lower(Key),
    Value = proplists:get_value(LowercasedKey, PropList),
    case Value of
        undefined ->
            throw({error, Key ++ "#invalid " ++ ErrorMsg});
        _ ->
            Value
    end.


% Make sure the procdict won't be rewritten some where else
% @TODO check if there's some way better than procdict
init_valid_region () ->
    lists:foreach(fun(X) -> put(X, true) end, ?LOCATIONS).
