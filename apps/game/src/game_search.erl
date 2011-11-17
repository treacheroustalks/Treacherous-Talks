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
%%%==================================================================

-module(game_search).

-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("eunit/include/eunit.hrl").
-export([search/1]).

%%-------------------------------------------------------------------
%% @doc
%% Performs a search on the game bucket
%% See http://wiki.basho.com/Riak-Search---Querying.html for query syntax
%% @end
%%-------------------------------------------------------------------
-spec search(string()) -> {ok, [integer()]} | {error, term()}.
search(Query) ->
    ?debugVal(Query),
    case db:search(?B_GAME, Query) of
        {ok, Results} ->
            {ok, get_game_ids(Results)};
        {error, Error} ->
            {error, Error}
    end.


%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
%% Remove bucket name from results
get_game_ids(Results) ->
    lists:reverse(get_game_ids(Results, [])).

get_game_ids([], Acc) ->
    Acc;
get_game_ids([[?B_GAME, BinId]|Rs], Acc) ->
    get_game_ids(Rs, [binary_to_integer(BinId)|Acc]).

binary_to_integer(B) ->
    list_to_integer(binary_to_list(B)).