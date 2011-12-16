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
-export([log_message/5]).

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

handle_call({get_reports, Role}, _From, State) ->
    Reports = get_reports(Role),
    {reply, Reports, State};

handle_call({mark_as_read, MessageId, Bucket}, _From, State) ->
    Result = do_mark_as_read(MessageId, Bucket),
    {reply, Result, State};

handle_call({mark_as_done, ReportId}, _From, State) ->
    Result = do_mark_as_read(ReportId, ?B_REPORT_MESSAGE_UNREAD),
    {reply, Result, State};
% user not allowed to send message to himself
handle_call({user_msg, #message{from_nick= Nick, to_nick= Nick}}, _From, State) ->
    {reply, {error, send_msg_to_yourself}, State};
handle_call({user_msg, Msg=#message{}}, _From, State) ->
    Result = case user_management:get(#user.nick, Msg#message.to_nick) of
                 {ok, {index_list, _IdxList}} ->
                     % Multiple nicks in the db, not allowed ...
                     {error, nick_not_unique};
                 {ok, #user{role = disabled}} ->
                     {error, black_listed};
                 {ok, User} ->
                     ToID = User#user.id,
                     NewMsg = Msg#message{to_id = ToID,
                                          date_created = erlang:universaltime()},
                     Event = #push_event{type = off_game_msg, data = NewMsg},
                     case controller:sync_push_event(ToID, Event) of
                         {ok, success} ->
                             log_user_msg(undefined, NewMsg, read);
                         {error, _} ->
                             log_user_msg(undefined, NewMsg, unread)
                     end;
                 {error, does_not_exist} ->
                     {error, invalid_nick};
                 {error, Error} ->
                     {error, Error}
             end,
    {reply, Result, State};

handle_call({report_msg, Report = #report_message{}}, _From, State) ->
    ReportMsg = Report#report_message{date_created = erlang:universaltime()},
    Result = log_report_msg(undefined, ReportMsg),
    {reply, Result, State};

handle_call({get_all_game_msg, GameId}, _From, State) ->
    Result = get_all_game_msg(GameId),
    {reply, Result, State};

handle_call({get_game_msg_by_phase, GameId, Year, Season, Phase}, _From, State) ->
    Result = get_game_msg_by_phase(GameId, Year, Season, Phase),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    ?DEBUG("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast({game_msg, GMsg = #game_message{}}, State) ->
    Event = #push_event{type = in_game_msg, data = GMsg},
    case controller:sync_push_event(GMsg#game_message.to_id, Event) of
        {ok, success} ->
            {ok, _ID} = log_game_msg(undefined, GMsg, read);
        {error, _} ->
            {ok, _ID} = log_game_msg(undefined, GMsg, unread)
        end,
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
log_user_msg(undefined, Msg = #message{from_id=From, to_id=To}, Status) ->
    Id = integer_to_list(db:get_unique_id()) ++ "-" ++
        integer_to_list(From) ++ "-" ++
        integer_to_list(To),
    log_message(Id, Msg#message{id = Id},
                ?B_MESSAGE, ?B_MESSAGE_UNREAD, Status).

log_game_msg(undefined,
             GMsg = #game_message{from_id=From, to_id=To,
                                  game_id=GameId, year=Year,
                                  season=Season, phase=Phase},
             Status) ->
    Id = integer_to_list(db:get_unique_id()) ++ "-" ++
        integer_to_list(From) ++ "-" ++
        integer_to_list(To) ++ "-" ++
        integer_to_list(GameId) ++ "-" ++
        integer_to_list(Year) ++ "-" ++
        atom_to_list(Season) ++ "-" ++
        atom_to_list(Phase),
    log_message(Id, GMsg#game_message{id = Id},
                ?B_GAME_MESSAGE, ?B_GAME_MESSAGE_UNREAD, Status).

log_report_msg(undefined, Report = #report_message{to=To}) ->
    Id = integer_to_list(db:get_unique_id()) ++ "-" ++
        atom_to_list(To),
    log_message(Id, Report#report_message{id = Id},
                ?B_REPORT_MESSAGE, ?B_REPORT_MESSAGE_UNREAD, unread).

do_unread(UserId) ->
    {ok, UserMsges} = get_unread_msges(UserId, ?B_MESSAGE,
                                       ?B_MESSAGE_UNREAD),
    {ok, GameMsges} = get_unread_msges(UserId, ?B_GAME_MESSAGE,
                                       ?B_GAME_MESSAGE_UNREAD),
    {ok, {UserMsges, GameMsges}}.


get_unread_msges(UserId, Bucket, UnreadBucket) ->
    Query = [ [<<"tokenize">>, <<"-">>, 3], [<<"eq">>, db:int_to_bin(UserId)]],
    {ok, Unread} = db:get_key_filter(UnreadBucket, Query),
    Keys = lists:map(fun(#unread{id=Id}) ->
                             list_to_binary(Id)
                        end, Unread),
    db:get_values(Bucket, Keys).

get_reports(Role) ->
    Query = [ [<<"tokenize">>, <<"-">>, 2],
              [<<"eq">>, list_to_binary(atom_to_list(Role))]],
    {ok, Unread} = db:get_key_filter(?B_REPORT_MESSAGE_UNREAD, Query),
    Keys = lists:map(fun(#unread{id=Id}) ->
                             list_to_binary(Id)
                        end, Unread),
    db:get_values(?B_REPORT_MESSAGE, Keys).


do_mark_as_read(MessageId, Bucket)
  when Bucket == ?B_MESSAGE_UNREAD;
       Bucket == ?B_GAME_MESSAGE_UNREAD;
       Bucket == ?B_REPORT_MESSAGE_UNREAD ->
    db:delete(Bucket, list_to_binary(MessageId)),
    {ok, marked_as_read}.

%% This function gets id, record and bucket name and convert the record
%% to proplist and stores it in the db.
log_message(Id, Msg, Bucket, UnreadBucket, Status) ->
    BinId = list_to_binary(Id),
    Unread = #unread{id = Id},

    DbObj = db_obj:create(Bucket, BinId, Msg),
    db:put(DbObj, [{w,0}]),

    case Status of
        unread ->
            DbObjUnread = db_obj:create(UnreadBucket, BinId, Unread),
            db:put(DbObjUnread, [{w,0}]);
        read ->
            ok
    end,
    {ok, Id}.


get_all_game_msg(GameId) ->
    Query = [ [<<"tokenize">>, <<"-">>, 4], [<<"eq">>, db:int_to_bin(GameId)]],
    db:get_key_filter(?B_GAME_MESSAGE, Query).

get_game_msg_by_phase(GameId, Year, Season, Phase) ->
    Queries =[ [[<<"tokenize">>, <<"-">>, 4], [<<"eq">>, db:int_to_bin(GameId)]] ],
    Queries2 = case Year of
                  undefined ->
                      Queries;
                  _ ->
                       [ [[<<"tokenize">>, <<"-">>, 5],
                          [<<"eq">>, db:int_to_bin(Year)]] | Queries]
              end,
    Queries3 = case Season of
                  undefined ->
                      Queries2;
                  _ ->
                      [ [[<<"tokenize">>, <<"-">>, 6],
                         [<<"eq">>, atom_to_bin(Season)]] | Queries2]
              end,
    Queries4 = case Phase of
                  undefined ->
                      Queries3;
                  _ ->
                      [ [[<<"tokenize">>, <<"-">>, 7],
                         [<<"eq">>, atom_to_bin(Phase)]] | Queries3]
              end,
    Query = case Queries4 of
                [Q1] -> Q1;
                _ ->
                    [[<<"and">> | Queries4]]
            end,
    db:get_key_filter(?B_GAME_MESSAGE, Query).


atom_to_bin(Atom) ->
    list_to_binary(atom_to_list(Atom)).
