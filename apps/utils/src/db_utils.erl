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
%%% @doc Common functions for database
%%%
%%% @since : 2 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(db_utils).

-export([
         get_search_term/3,
         do_search_values/3,
         do_search/2
        ]).

-include_lib ("datatypes/include/game.hrl").
-include_lib ("datatypes/include/user.hrl").

%%-----------------------------------------------------------------
%% @doc
%%   this function get the field number, its corresponding value
%%   and record name and return a search term
%% @end
%%-----------------------------------------------------------------
-spec get_search_term(integer(), any(), atom() | list() ) -> {ok, string()}.
get_search_term(Field, Val, RecordName) when is_atom(RecordName) ->
    RecFields = case RecordName of
                    game -> record_info(fields, game);
                    user -> record_info(fields, user)
                end,
    get_search_term(Field, Val, RecFields);
get_search_term(Field, Val, Fields) when Field < length(Fields) ->
    FieldAtom = lists:nth(Field-1, Fields),
    case is_list(Val) of
        true ->
            {ok, lists:flatten(io_lib:format("~p=~s", [FieldAtom, Val]))};
        false ->
            {ok, lists:flatten(io_lib:format("~p=~p", [FieldAtom, Val]))}
    end.

%%-----------------------------------------------------------------
%% @doc
%%   this function get a query, bucket and record name and return the list of
%%   record which match the query.
%%
%%   Note: whenever we have sibling in search result, it will return
%%   empty list.
%% @end
%%-----------------------------------------------------------------
-spec do_search_values(binary(), string(), atom()) ->
          {ok, [#user{}| #game{}]} |
          {error, term()}.
do_search_values(Bucket, Query, RecordName) ->
    case db:search_values(Bucket, Query) of
        {ok, PropLists} ->
            Results = lists:map(fun(PropList) ->
                                        data_format:plist_to_rec(RecordName, PropList)
                                end,
                                PropLists),
            {ok, Results};
        Error ->
            Error
    end.

%%-----------------------------------------------------------------
%% @doc
%%   this function get a query and bucket return the list of id
%%   of record which match the query.
%% @end
%%-----------------------------------------------------------------
-spec do_search( binary(), string()) ->
          {ok, [integer()]} |
          {error, term()}.
do_search(Bucket, Query) ->
    case db:search(Bucket, Query) of
        {ok, Results} ->
            Result1 = lists:map(fun([_, Key]) -> binary_to_integer(Key)
                                end, Results),
            {ok, Result1};
        Error ->
            Error
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
binary_to_integer(B) ->
    list_to_integer(binary_to_list(B)).