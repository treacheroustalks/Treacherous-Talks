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
%%% @doc This mdoules provides the interface for sessions.
%%%
%%% @end
%%%
%%% @since :  2 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------
-export([
         start/2,
         stop/1,
         alive/1,
         logout/2,
         get_session_user/2,
         update_user/2,
         create_game/2,
         get_game/2,
         reconfig_game/2,
         game_overview/2,
         join_game/2,
         game_order/2
        ]).

%% ------------------------------------------------------------------
%% Internal Function Exports - for eUnit only!
%% ------------------------------------------------------------------
-export([
        ]).

%% ------------------------------------------------------------------
%% Internal macros
%% ------------------------------------------------------------------
-define(SESSION_CAST(Id, Info), gen_server:cast(session_id:to_pid(Id), Info)).
-define(SESSION_CAST(Id, Cmd, Data), ?SESSION_CAST(Id, {Cmd, Data})).

-define(SESSION_CALL(Id, Info), gen_server:call(session_id:to_pid(Id), Info)).
-define(SESSION_CALL(Id, Cmd, Data), ?SESSION_CALL(Id, {Cmd, Data})).

%% ------------------------------------------------------------------
%% Interface Function Implementation
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Starts a new session for the given user and returns the session id.
%%
%% @spec start(User::#user{}, #session_history{}) -> string()
%% @end
%%-------------------------------------------------------------------
start(User=#user{}, Hist) ->
    {ok, Pid} = session_proc:start(User, Hist),
    session_id:from_pid(Pid).


%%-------------------------------------------------------------------
%% @doc
%% Stops a session for the given user.
%%
%% @spec stop(User::#user{}) -> ok
%% @end
%%-------------------------------------------------------------------
stop(SessionId) ->
    ?SESSION_CAST(SessionId, stop).
logout(SessionId, _Arg) ->
    stop(SessionId).

%%-------------------------------------------------------------------
%% @doc
%% Checks if a session with the given ID exists
%%
%% @spec alive(SessionId::list()) -> boolean()
%% @end
%%-------------------------------------------------------------------
alive(SessionId) ->
    case session_id:to_pid(SessionId) of
        {error, _} ->
            false;
        Pid ->
              case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                  {badrpc, _Reason} -> false;
                  Result -> Result
              end
    end.

%%-------------------------------------------------------------------
%% @doc get_session_user/2
%% API for getting the session user
%%
%% @spec get_session_user(string(), any()) -> #user{}
%% @end
%%-------------------------------------------------------------------
get_session_user(SessionId, _) ->
    ?SESSION_CALL(SessionId, get_session_user).

%%-------------------------------------------------------------------
%% @doc update_user/2
%% API for updating a user
%%
%% @spec update_user(string(), #user{}) -> #user{}
%% @end
%%-------------------------------------------------------------------
update_user(SessionId, PropList) ->
    ?SESSION_CALL(SessionId, update_user, PropList).

%%-------------------------------------------------------------------
%% @doc create_game/2
%% API for creation of a game
%%
%% @spec create_game(string(), #game{}) -> #game{}
%% @end
%%-------------------------------------------------------------------
create_game(SessionId, Game) ->
    ?SESSION_CALL(SessionId, create_game, Game).

%%-------------------------------------------------------------------
%% @doc get_game/2
%% API to get a game
%%
%% @spec get_game(string(), #game{}) -> #game{}
%% @end
%%-------------------------------------------------------------------
get_game(SessionId, GameId) ->
    ?SESSION_CALL(SessionId, get_game, GameId).

%%-------------------------------------------------------------------
%% @doc reconfig_game/2
%% API for updating a game
%%
%% @spec reconfig_game(string(), {integer(), #game{}}) -> #game{}
%% @end
%%-------------------------------------------------------------------
reconfig_game(SessionId, Data = {_GameId, _PropList}) ->
    ?SESSION_CALL(SessionId, reconfig_game, Data).

%%-------------------------------------------------------------------
%% @doc game_overview/2
%% API for updating a game
%%
%% @spec game_overview(string(), integer()) -> #game_overview{}
%% @end
%%-------------------------------------------------------------------
game_overview(SessionId, GameId) ->
    ?SESSION_CALL(SessionId, game_overview, GameId).

%%-------------------------------------------------------------------
%% @doc join_game/2
%% API for updating a game
%%
%% @spec join_game(string(), {integer(), atom()}) ->
%%         {ok, integer()} | {error, country_not_available}
%% @end
%%-------------------------------------------------------------------
join_game(SessionId, Data = {_GameId, _Country}) ->
    ?SESSION_CALL(SessionId, join_game, Data).

%%-------------------------------------------------------------------
%% @doc game_order/2
%% API for sending a list game order
%%
%% @end
%%-------------------------------------------------------------------
game_order(SessionId, Data = {_GameId, _OrderList}) ->
    ?SESSION_CALL(SessionId, game_order, Data).

