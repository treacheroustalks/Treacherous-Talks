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
%%% @doc Manages the session presence.
%%% Stores all session in a mnesia table.
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_presence).

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------
-export([
         init/0,
         add/3,
         remove/1,
         handle_corpse/1,
         is_online/1,
         get_session_id/1,
         get_all_by_type/1,
         get_all/0,
         count_all_by_type/1,
         count_all/0
        ]).

-include_lib("utils/include/debug.hrl").
-include_lib ("datatypes/include/bucket.hrl").

%% ------------------------------------------------------------------
%% Internal macros and records
%% ------------------------------------------------------------------
-define(TABLE, session).
-record(session, {user_id :: integer() | '_',
                  session_id :: string() | '_',
                  client_type :: im | mail | web | '_'
                 }).
%% ------------------------------------------------------------------
%% Interface Function Implementation
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Initializes the mnesia table
%% if there is an item `{backend_nodes, [ nodes() ]' in the app.config,
%% init will ping them and try to replicate the session table from the first
%% one that reacts. Only if that list does not exist or no one reacts to a ping,
%% a local session table will be created.
%% This local table can furtheron be used by other starting backends as a
%% replication source
%%
%% @spec init() -> ok | {aborted, Reason}
%% @end
%%-------------------------------------------------------------------
init() ->
    case mnesia_utils:replicate (session) of
        {error, no_reachable_backend} ->
            ?DEBUG ("didn't find responding backends.~n"),
            ?DEBUG ("creating fresh session table.~n"),
            case mnesia:create_table(?TABLE, [{type, set},
                                              {attributes,
                                               record_info(fields, session)}])
            of
                {atomic, ok} ->
                    case mnesia:add_table_index(session, client_type)
                    of
                        {atomic, ok} ->
                            ok;
                        Other ->
                            Other
                    end;
                {aborted, {already_exists, ?TABLE}} ->
                    ok;
                Else ->
                    Else
                end;
        ok ->
            ok
    end.

%%-------------------------------------------------------------------
%% @doc
%% Adds a user to the session presence.
%%
%% @spec add(UserId::integer(), SessionId::string(),
%%           ClientType::im | mail | web) ->
%%           ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
add(UserId, SessionId, ClientType) ->
    ?DEBUG ("add"),
    corpses:save_corpse (?MODULE, UserId, UserId),
    AddFun = fun() ->
                     mnesia:write(#session{user_id = UserId,
                                           session_id = SessionId,
                                           client_type = ClientType})
             end,
    case mnesia:transaction(AddFun) of
        {atomic, ok} ->
            ok;
        Error ->
            {error, Error}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Removes a user from the session presence.
%%
%% @spec remove(UserId::integer()) ->
%%           ok | {error, not_online} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
remove(UserId) ->
    DelFun = fun() ->
                     case mnesia:read({session, UserId}) of
                         [] ->
                             not_online;
                         [_Session] ->
                             mnesia:delete({session, UserId})
                     end
             end,
    case mnesia:transaction(DelFun) of
        {atomic, ok} ->
            db:delete (?B_CORPSES,
                       list_to_binary (
                         atom_to_list (?MODULE) ++
                         io_lib:format ("~p", [UserId]))),
            ok;
        {atomic, not_online} ->
            {error, not_online};
        Error ->
            {error, Error}
    end.

handle_corpse({_Key, UserId}) ->
    ?DEBUG ("handle_corpse(~p)~n", [UserId]),
    remove(UserId).

%%-------------------------------------------------------------------
%% @doc
%% Checks if a user is online
%%
%% @spec is_online(UserId::integer()) -> boolean()
%% @end
%%-------------------------------------------------------------------
is_online(UserId) ->
    case mnesia:dirty_read({session, UserId}) of
        [] ->
            false;
        [_Session] ->
            true
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets the session id for a user.
%%
%% @spec get_session_id(UserId::integer()) ->
%%           {ok, string()} | {error, not_online}
%% @end
%%-------------------------------------------------------------------
get_session_id(UserId) ->
    case mnesia:dirty_read({session, UserId}) of
        [] ->
            {error, not_online};
        [Session] ->
            {ok, Session#session.session_id}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets user id and session id of all online users, example:
%% [{UserId, SessionId, ClientType}] = get_all()
%%
%% @spec get_all() ->
%%           [{integer(), string(), im | mail | web}]
%% @end
%%-------------------------------------------------------------------
get_all() ->
    All = mnesia:dirty_match_object(#session{user_id = '_',
                                             session_id = '_',
                                             client_type = '_'}),
    lists:map(fun(#session{user_id = UserId,
                           session_id = SessionId,
                           client_type = ClientType}) ->
                  {UserId, SessionId, ClientType}
              end, All).

%%-------------------------------------------------------------------
%% @doc
%% Gets user id and session id of all online users by their
%% interface type, example:
%% [{UserId, SessionId}] = get_all_by_type(ClientType)
%%
%% @spec get_all_by_type(im | mail | web) ->
%%           [{integer(), string()}]
%% @end
%%-------------------------------------------------------------------
get_all_by_type(ClientType) ->
    All = mnesia:dirty_index_read(?TABLE, ClientType, client_type),
    lists:map(fun(#session{user_id = UserId,
                           session_id = SessionId}) ->
                  {UserId, SessionId}
              end, All).

%%-------------------------------------------------------------------
%% @doc
%% Gives number of all online users, example:
%% Number = count_all()
%%
%% @spec count_all() ->
%%           integer()
%% @end
%%-------------------------------------------------------------------
count_all() ->
    mnesia:table_info(?TABLE,size).

%%-------------------------------------------------------------------
%% @doc
%% Gives number of all online users of particular interface, example:
%% Number = count_all_by_type(ClientType)
%%
%% @spec count_all_by_type(im | mail | client) ->
%%           integer()
%% @end
%%-------------------------------------------------------------------
count_all_by_type(ClientType) ->
    length(get_all_by_type(ClientType)).
