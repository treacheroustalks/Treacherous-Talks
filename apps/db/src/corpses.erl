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
-compile([{parse_transform, lager_transform}]).

-export ([get_corpses/1, save_corpse/2, save_corpse/3,
          delete_corpse/1, delete_corpse/3]).

-include_lib ("datatypes/include/bucket.hrl").
-include_lib ("utils/include/debug.hrl").

%%-------------------------------------------------------------------
% @doc
% returns all corpses that belong to a given backend
% @end
%%-------------------------------------------------------------------
-spec get_corpses (DeadBackend :: node ()) -> [necromancer:corpse ()] | [].
get_corpses (DeadBackend) when is_atom (DeadBackend) ->
    lager:info("DeadBackend = ~p", [DeadBackend]),
    Query = create_node_query(DeadBackend),
    case db:get_key_filter(?B_CORPSES, Query) of
        {ok, Corpses} -> Corpses;
        _ -> []
    end.

save_corpse (Module, Data) ->
    save_corpse (Module, make_ref (), Data).

%%-------------------------------------------------------------------
% @doc
% saves one corpse to the database with the key "`Module++Ref'"
% `handle_corpse' will be called with `{Key, Data}' where `Key'
% is the database-key and `Data' is your Data.
% `Ref' has to be uniqe within the handler-module.
% @end
%%-------------------------------------------------------------------
-spec save_corpse (module (), Ref :: any (), Data :: any ()) -> ok.
save_corpse (Module, Ref, Data) ->
    lager:debug("saving corpse for module = ~p Data = ~p", [Module, Data]),
    Key = create_key(node(), Module, Ref),
    BinKey = list_to_binary(Key),
    DbData = {Module, {Key, Data}},
    DBO = db_obj:create(?B_CORPSES, BinKey, DbData),
    db:put(DBO, [{w, 0}]),
    ok.



%%-------------------------------------------------------------------
%% @doc
%% Deletes a corpse from the database.
%% @end
%%-------------------------------------------------------------------
-spec delete_corpse(Key :: string()) -> ok.
delete_corpse(Key) ->
    BinKey = list_to_binary(Key),
    db:delete(?B_CORPSES, BinKey),
    ok.

-spec delete_corpse(node(), module(), Ref :: any()) -> ok.
delete_corpse(Node, Module, Ref) ->
    Key = create_bin_key(Node, Module, Ref),
    db:delete(?B_CORPSES, Key),
    ok.


%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
-spec create_key(node(), module(), Ref :: any()) -> string().
create_key(Node, Mod, Ref) ->
    atom_to_list(Node) ++ "-" ++
    atom_to_list(Mod) ++ "-" ++
    lists:flatten(io_lib:format("~p", [Ref])).

-spec create_bin_key(node(), module(), Ref :: any()) -> binary().
create_bin_key(Node, Mod, Ref) ->
    list_to_binary(create_key(Node, Mod, Ref)).

-spec create_node_query(node()) -> any().
create_node_query(Node) ->
    [[<<"starts_with">>, list_to_binary(atom_to_list(Node)) ]].
