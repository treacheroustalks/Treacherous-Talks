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
%%% @TODO Add specs
%%%-------------------------------------------------------------------
-module(game_join_proc_map).

-export([init/0, get_pid/1, store_pid/2, delete_pid/1]).

-include("include/game.hrl").
-include_lib("utils/include/debug.hrl").

%% I think this should be called during the node boot
%mnesia:start().
%% I think this isn't needed since we only want in-ram
%mnesia:create_schema([node()]).


%%---------------------------------------------------------
%% I think this should be called during the game app startup
%%---------------------------------------------------------
init() ->
    case mnesia_utils:replicate (game_join_proc) of
        {error, no_reachable_backend} ->
            ?DEBUG("didn't find responding backends.~n"),
            ?DEBUG("creating fresh game_join_proc table~n"),
            case mnesia:create_table(game_join_proc,
                                     [{attributes,
                                       record_info(fields, game_join_proc)}]) of
                {atomic, ok} ->
                    ok;
                {aborted,{already_exists,game_join_proc}} ->
                    ok;
                Else ->
                    Else
            end;
        ok ->
            ok
    end.


%%---------------------------------------------------------
%% get_pid(GameId) -> Pid | none | Error
%%---------------------------------------------------------
get_pid(GameId) ->
    Fun = fun() ->
                  case mnesia:read(game_join_proc, GameId) of
                      [] ->
                          none;
                      [#game_join_proc{pid=Pid}] ->
                          Pid
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        Error ->
            Error
    end.


%%---------------------------------------------------------
%% store_pid(GameId, Pid) -> ok | Error
%%
%% ok => the given pid was stored as the only record
%% for this game id.
%%
%% Error => the given pid was not stored. The reason should
%% be apparrent from the value of Error
%% Error = {atomic,{error,existing_is_alive}} means there
%%   was already a record for GameId and the Pid there
%%   is a live process.
%%---------------------------------------------------------
store_pid(GameId, Pid) ->
    Fun = fun() ->
                  case deal_with_existing(GameId) of
                      ok ->
                          Game2Pid = #game_join_proc{game_id=GameId,
                                                     pid=Pid},
                          mnesia:write(Game2Pid);
                      Error ->
                          Error
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        Error ->
            Error
    end.


delete_pid(GameId) ->
    Fun = fun() ->
                  mnesia:delete({game_join_proc, GameId})
          end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        Error ->
            Error
    end.


%%------------------------------------------------------
%% Internal functions
%%------------------------------------------------------

%% If no record exists for given game, ok
%% If record exists
%%   If process exists, error
%%   Else, delete record, ok
%%
%% !!!!! Expects to be called within a mnesia:transaction
deal_with_existing(GameId) ->
    case mnesia:read(game_join_proc, GameId) of
        [] ->
            ok;
        [#game_join_proc{pid=Pid}] ->
            case game_join_proc:is_alive(Pid) of
                true ->
                    {error, existing_is_alive};
                false ->
                    mnesia:delete(game_join_proc, GameId),
                    ok
            end
    end.
