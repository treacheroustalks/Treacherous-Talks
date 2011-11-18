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
%%% @doc Module to search games using Riak search
%%%
%%% @since : 16 Nov 2011 by Bermuda Triangle
%%%
%%% See http://wiki.basho.com/Riak-Search---Querying.html for query syntax
%%%==================================================================

-module(game_search).

-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("datatypes/include/game.hrl").
-export([search/1, search_values/1]).

%%-------------------------------------------------------------------
%% @doc
%% Performs a search on the game bucket
%% @end
%%-------------------------------------------------------------------
-spec search(string()) -> {ok, [integer()]} | {error, term()}.
search(Query) ->
    case db:search(?B_GAME, Query) of
        {ok, Results} ->
            Result1 = lists:map(fun([_, Key]) -> binary_to_integer(Key)
                                end, Results),
            {ok, Result1};
        Error ->
            Error
    end.


%%-------------------------------------------------------------------
%% @doc
%% Performs a search on the game bucket and returns the games records
%% @end
%%-------------------------------------------------------------------
-spec search_values(string()) -> {ok, [#game{}]} | {error, term()}.
search_values(Query) ->
    case db:search_values(?B_GAME, Query) of
        {ok, Games} ->
            % Convert game proplists to game records
            {ok, lists:map(fun(GamePropList) ->
                                   data_format:plist_to_rec(?GAME_REC_NAME,
                                                            GamePropList)
                           end, Games)};
        Error ->
            Error
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
binary_to_integer(B) ->
    list_to_integer(binary_to_list(B)).
