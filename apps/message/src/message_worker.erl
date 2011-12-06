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
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc The business end of the message app.
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(message_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% export for eunit
-export([log_message/3]).

-include_lib("utils/include/debug.hrl").

-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/message.hrl").

%% server state
-record(state, {}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

ping() ->
    gen_server:call(service_worker:select_pid(?MODULE), ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};

handle_call({unread, UserId}, _From, State) ->
    Unread = do_unread(UserId),
    {reply, Unread, State};

handle_call({mark_as_read, MessageId, Bucket}, _From, State) ->
    Result = do_mark_as_read(MessageId, Bucket),
    {reply, Result, State};

handle_call({user_msg, Msg=#message{}}, _From, State) ->
    Result = case user_management:get(#user.nick, Msg#message.to_nick) of
                 {ok, {index_list, _IdxList}} ->
                     % Multiple nicks in the db, not allowed ...
                     {error, nick_not_unique};
                 {ok, User} ->
                     ToID = User#user.id,
                     NewMsg = Msg#message{to_id = ToID,
                                          date_created = erlang:universaltime()},
                     LogResult = log_user_msg(undefined, NewMsg),
                     Event = #push_event{type = off_game_msg, data = NewMsg},
                     controller:push_event(ToID, Event),
                     LogResult;
                 {error, does_not_exist} ->
                     {error, invalid_nick};
                 {error, Error} ->
                     {error, Error}
             end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    ?DEBUG("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast({game_msg, GMsg = #game_message{}}, State) ->
    {ok, _ID} = log_game_msg(undefined, GMsg),
    Event = #push_event{type = in_game_msg, data = GMsg},
    controller:push_event(GMsg#game_message.to_id, Event),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?DEBUG("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
log_user_msg(undefined, Msg = #message{}) ->
    ID = db:get_unique_id(),
    log_message(ID, Msg#message{id = ID}, ?B_MESSAGE).

log_game_msg(undefined, GMsg = #game_message{}) ->
    ID = db:get_unique_id(),
    log_message(ID, GMsg#game_message{id = ID}, ?B_GAME_MESSAGE).

do_unread(UserId) ->
    {ok, UserMsges} = get_unread_msges(UserId, ?B_MESSAGE, message),
    {ok, GameMsges} = get_unread_msges(UserId, ?B_GAME_MESSAGE, game_message),
    {ok, {UserMsges, GameMsges}}.


get_unread_msges(UserId, Bucket, RecordName) ->
    Query = lists:flatten(io_lib:format("to_id=~p AND status=unread", [UserId])),
    {ok, PropLists} = db:search_values(Bucket, Query),
    Messages = lists:map(fun(PropList) ->
                                 data_format:plist_to_rec(RecordName, PropList)
                         end,
                         PropLists),
    {ok, Messages}.

do_mark_as_read(MessageId, Bucket)
  when Bucket == ?B_MESSAGE;
       Bucket == ?B_GAME_MESSAGE ->
    case db:get(Bucket, db:int_to_bin(MessageId)) of
        {ok, DBObj} ->
            PropList = db_obj:get_value(DBObj),
            % modify via the record format to maintain the property order easily
            UpdatedPropList =
                case Bucket of
                    ?B_MESSAGE ->
                        Message = data_format:plist_to_rec(message, PropList),
                        ReadMessage = Message#message{status=read},
                        data_format:rec_to_plist(ReadMessage);
                    ?B_GAME_MESSAGE ->
                        Message = data_format:plist_to_rec(game_message, PropList),
                        ReadGameMessage = Message#game_message{status=read},
                        data_format:rec_to_plist(ReadGameMessage)
                end,
            ReadDBObj = db_obj:set_value(DBObj, UpdatedPropList),
            db:put(ReadDBObj),
            ok;
        {error, notfound} ->
            {error, notfound}
    end.

%%------------------------------------------------------------------------------
%%  @doc
%%    this function gets id, record and bucket name and convert the record
%%    to proplist and stor.
%%  @end
%%------------------------------------------------------------------------------
log_message(ID, Msg, Bucket) ->
    BinID = db:int_to_bin(ID),
    % convert record to proplist to be able to do search
    MsgPropList = data_format:rec_to_plist(Msg),
    DbObj = db_obj:create(Bucket, BinID, MsgPropList),
    db:put(DbObj),
    {ok, ID}.
