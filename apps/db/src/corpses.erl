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

-module (corpses).

-export ([get_corpses/1, save_corpse/2, save_corpse/3]).

-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("utils/include/debug.hrl").

%% -----------------------------------------------------------------------------
% @doc
% returns all corpses that belong to a given backend
% @end
%% -----------------------------------------------------------------------------
-spec get_corpses (DeadBackend :: node ()) -> [necromancer:corpse ()] | [].
get_corpses (DeadBackend) when is_atom (DeadBackend) ->
    ?DEBUG ("get_corpses(~p)~n", [DeadBackend]),
    Query = "node=" ++ atom_to_list(node()),
    case db:search_values(?B_CORPSES, Query) of
        {ok, Corpses} ->
            lists:map(fun(Corpse) ->
                              {data, Data} = lists:keyfind(data, 1, Corpse),
                              Data
                      end, Corpses);
        _ ->
            []
    end.

save_corpse (Module, Data) ->
    save_corpse (Module, make_ref (), Data).

%% -----------------------------------------------------------------------------
% @doc
% saves one corpse to the database with the key "`Module++Ref'"
% `handle_corpse' will be called with `{Key, Data}' where `Key'
% is the database-key and `Data' is your Data.
% `Ref' has to be uniqe within the handler-module.
% @end
%% -----------------------------------------------------------------------------
-spec save_corpse (module (), Ref :: any (), Data :: any ()) -> ok.
save_corpse (Module, Ref, Data) ->
    ?DEBUG ("save_corpse(~p, ~p)~n", [Module, Data]),
    Key = lists:flatten(io_lib:format ("~p~p", [Module, Ref])),
    BinKey = list_to_binary(Key),
    Node = atom_to_list(node()),
    DbData = {Module, {Key, Data}},
    DBO = db_obj:create(?B_CORPSES, BinKey, [{node, Node}, {data, DbData}]),
    db:put(DBO, [{w, 0}]),
    ok.
