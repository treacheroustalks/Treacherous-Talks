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

% @doc
% returns all corpses that belong to a given backend
% @end
-spec get_corpses (DeadBackend :: node ()) -> [necromancer:corpse ()] | [].
get_corpses (DeadBackend) when is_atom (DeadBackend) ->
    ?DEBUG ("get_corpses(~p)~n", [DeadBackend]),
    {ok, CorpsesBucketKeys} =
        db:get_index (?B_CORPSES,
                      {<<"node_bin">>, atom_to_binary (DeadBackend, latin1)}),
    CorpsesKeys = lists:map (fun ([?B_CORPSES, Key]) ->
                                 Key
                         end, CorpsesBucketKeys),
    case db:get_values (?B_CORPSES, CorpsesKeys) of
        {ok, Corpses} ->
            Corpses;
        _ ->
            []
    end.

save_corpse (Module, Data) ->
    save_corpse (Module, make_ref (), Data).

% @doc
% saves one corpse to the database with the key "`Module++Ref'"
% @end
-spec save_corpse (module (), Ref :: any (), Data :: any ()) -> ok.
save_corpse (Module, Ref, Data) ->
    ?DEBUG ("save_corpse(~p, ~p)~n", [Module, Data]),
    Key = list_to_binary (atom_to_list (Module) ++
                          io_lib:format ("~p", [Ref])),
    DBO = db_obj:create (?B_CORPSES, Key, {Module, {Key, Data}}),
    DBOI = db_obj:set_indices (DBO,
                               [{<<"node_bin">>,
                                 atom_to_binary (node (), latin1)}]),
    db:put (DBOI),
    ok.
