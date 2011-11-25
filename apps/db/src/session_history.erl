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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc This module provides functions for the session history.
%%%
%%% @end
%%%
%%% @since :  2 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_history).

-include_lib("datatypes/include/bucket.hrl").

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------
-export([
         create/1,
         delete/1,
         id/1,
         history/1,
         latest/1,
         add/2,
         db_put/1,
         db_update/1,
         db_get/1,
         resolve_history_siblings/1,
         find_newest/2,
         resolve_conflict/3
        ]).

%% ------------------------------------------------------------------
%% Types and records.
%% ------------------------------------------------------------------
-type id() :: integer().
-record(session_history, {id :: id(),
                          history = [] :: list()}).

%% ------------------------------------------------------------------
%% Interface Function Implementation
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Creates a new session_history object.
%%
%% @spec create(id()) -> #session_history{}
%% @end
%%-------------------------------------------------------------------
create(Id) ->
    #session_history{id = Id}.

%%-------------------------------------------------------------------
%% @doc
%% Deletes a session_history object.
%%
%% @spec delete(#session_history{}) -> ok
%% @end
%%-------------------------------------------------------------------
delete(_History) ->
    ok.

%%-------------------------------------------------------------------
%% @doc
%% Gets the id of the user, the session history objects belongs to.
%%
%% @spec id(#session_history{}) -> id()
%% @end
%%-------------------------------------------------------------------
id(#session_history{id = Id}) ->
    Id.

%%-------------------------------------------------------------------
%% @doc
%% Getter and setter for the history.
%% @end
%%-------------------------------------------------------------------
history(#session_history{history = History}) ->
    History.

history(Sess = #session_history{}, NewVal) ->
    Sess#session_history{history = NewVal}.

%%-------------------------------------------------------------------
%% @doc
%% Get the latest value from the session history.
%%
%% @spec latest(#session_history{}) -> {ok, id()} | history_empty
%% @end
%%-------------------------------------------------------------------
latest(#session_history{history = History}) ->
    latest_from_list(History).

latest_from_list([Session | _History]) ->
    {ok, Session};
latest_from_list([]) ->
    history_empty.

%%-------------------------------------------------------------------
%% @doc
%% Adds a new session to the history.
%%
%% @spec add(#session_history{}, string()) -> #session_history{}
%% @end
%%-------------------------------------------------------------------
add(Rec = #session_history{history = History}, Session) when is_list(Session) ->
    history(Rec, [Session|History]).

%%-------------------------------------------------------------------
%% @doc
%% Writes a session history object to the database
%%
%% @spec db_put(#session_history{}) -> ok | {error, any()}
%% @end
%%-------------------------------------------------------------------
db_put(Sess = #session_history{id = Id}) ->
    BinId = db:int_to_bin(Id),
    DbObj = db_obj:create(?B_SESSION_HISTORY, BinId, Sess),
    db:put(DbObj, [{w, all}]).

%%-------------------------------------------------------------------
%% @doc
%% Updates a session history object in the database
%%
%% @spec db_update(#db_obj{}) -> ok | {error, any()}
%% @end
%%-------------------------------------------------------------------
db_update(DbObj) ->
    db:put(DbObj, [{w, all}]).

%%-------------------------------------------------------------------
%% @doc
%% Reads a session history object from the database.
%%
%% @spec db_get(id()) -> {ok, #db_obj{}} | {error, does_not_exist}
%% @end
%%-------------------------------------------------------------------
db_get(Id) ->
    BinId = db:int_to_bin(Id),
    case db:get(?B_SESSION_HISTORY, BinId, [{r, all}]) of
        {ok, Obj} ->
            {ok, Obj};
        _Error ->
            {error, does_not_exist}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Resolves siblings in the session history
%%
%% @spec resolve_history_siblings(#db_obj{}) -> #db_obj{}
%% @end
%%-------------------------------------------------------------------
resolve_history_siblings(DbObj) ->
    case db_obj:has_siblings(DbObj) of
        true ->
            [HistObj|Siblings] = db_obj:get_siblings(DbObj),
            Hist = db_obj:get_value(HistObj),
            NewHist = lists:foldl(
                        fun(Sibling, History) ->
                                SibHist = db_obj:get_value(Sibling),
                                {ok, Latest} = latest(SibHist),
                                add(History, Latest)
                        end, Hist, Siblings),
            db_obj:set_value(HistObj, NewHist);
        false ->
            DbObj
    end.

%%-------------------------------------------------------------------
%% @doc
%% Given a list of sessions and a history find the newest of those.
%%
%% @spec find_newest(#session_history{}, list()) ->
%%         integer() | history_empty | not_in_history
%% @end
%%-------------------------------------------------------------------
find_newest(#session_history{history = History}, Sessions) ->
    case {History, Sessions} of
        {[], _} ->
            history_empty;
        {_, []} ->
            not_in_history;
        _ ->
            find_newest_list(History, Sessions)
    end.

find_newest_list([], _Sessions) ->
    not_in_history;
find_newest_list([H|History], Sessions) ->
    Find = fun(Sess, {false, Pos}) ->
                   case Sess == H of
                       true -> {true, Pos};
                       false -> {false, Pos+1}
                   end;
              (_, {true, Pos}) -> {true, Pos}
           end,
    case lists:foldl(Find, {false, 1}, Sessions) of
        {true, Pos} ->
            Pos;
        {false, _} ->
            find_newest_list(History, Sessions)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Resolves siblings with the help of the session history.
%% Field indicates the position of last_session in the record.
%%
%% @spec resolve_conflict(#session_history{},
%%                        #db_obj{},
%%                        integer()) ->
%%               #db_obj{}
%% @end
%%-------------------------------------------------------------------
resolve_conflict(Hist, DbObj, Field) ->
    case db_obj:has_siblings(DbObj) of
        false ->
            DbObj;
        true ->
            Siblings = db_obj:get_siblings(DbObj),
            LastSessions = lists:map(
                             fun(SibObj) ->
                                     element(Field, db_obj:get_value(SibObj))
                             end, Siblings),
            Pos = find_newest(Hist, LastSessions),
            lists:nth(Pos, Siblings)
    end.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------
