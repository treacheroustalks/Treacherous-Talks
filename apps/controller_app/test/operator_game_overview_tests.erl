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
%%% @doc Unit tests for reconfiguring games games.
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(operator_game_overview_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/message.hrl").

-export([tests/1, success/3, invalid/3]).

tests([Callback, SessId, GameId]) ->
    [
     ?_test(success(Callback, SessId, GameId)),
     ?_test(invalid(Callback, SessId, GameId))
    ].
%%-------------------------------------------------------------------
%% game overview tests
%%-------------------------------------------------------------------
success(Callback, SessId, GameId) ->
    ?debugMsg("GAMES_OVERVIEW TEST SUCCESS"),
    game_timer:sync_event(GameId, timeout),

    Cmd = {operator_game_overview, {ok, SessId, GameId}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, ResultData} = Result,
    {GOV, _Tree} = ResultData,
    {game_player, _,[{game_user, _,Country1},{game_user,_,Country2}]} =
        GOV#game_overview.players,
    ?assertEqual({germany,england}, {Country1,Country2}),
    ?assertEqual({operator_game_overview, success}, CmdRes),
    ?assertEqual(true, is_record(GOV, game_overview)),

    Msg1 = #game_message{game_id=7777, content="msg1", year = 1902,season=fall,phase=order,date_created=1,sender_country=england},
    put_game_msg(99991, Msg1),
    Key = "7777-1902-fall-order_phase-england",
    BinID = list_to_binary(Key),
    OrderList = [move,support,convoy,hold],
    DBGameOrderObj = db_obj:create (?B_GAME_ORDER, BinID, #game_order{order_list=OrderList}),
    db:put (DBGameOrderObj),

    Query = "game_id=7777 AND year=1902 AND season=fall AND phase=order AND sender_country=england",

    Cmd2 = {operator_get_game_msg, {ok, SessId, {Key, Query}}},
    Result2 = controller:handle_action(Cmd2, Callback),
    {CmdRes2, ResultData2} = Result2,
    %{GMsg, Order} = ResultData2,

    db:delete (?B_GAME_ORDER, BinID),
    del_game_msg(99991),
    ?assertEqual({operator_get_game_msg, success}, CmdRes2),
    ?assertEqual({[Msg1], OrderList}, ResultData2).

invalid(_Callback, _SessId, _GameId) ->
    ok.

%%-------------------------------------------------------------------
%% Help Functions
%%-------------------------------------------------------------------
put_game_msg(Key, Msg) ->
    BinID = db:int_to_bin(Key),
    % convert record to proplist to be able to do search
    MsgPropList = data_format:rec_to_plist(Msg),
    DbObj = db_obj:create(?B_GAME_MESSAGE, BinID, MsgPropList),
    db:put(DbObj).

del_game_msg(Key) ->
    BinKey = db:int_to_bin(Key),
    db:delete(?B_GAME_MESSAGE, BinKey).