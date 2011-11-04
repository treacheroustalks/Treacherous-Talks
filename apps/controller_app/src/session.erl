%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
         start/1,
         stop/1,
         alive/1,
         get_session_user/2,
         update_user/2,
         create_game/2,
         reconfig_game/2,
         game_overview/2,
         join_game/2
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
%% @spec start(User::#user{}) -> string()
%% @end
%%-------------------------------------------------------------------
start(User=#user{}) ->
    {ok, Pid} = session_proc:start(User),
    session_id:from_pid(Pid).
    

%%-------------------------------------------------------------------
%% @doc
%% Stops a session for the given user.
%%
%% @spec stop(User::#user{}) -> ok
%% @end
%%-------------------------------------------------------------------
stop(SessionId) ->
    exit(session_id:to_pid(SessionId), shutdown).
    
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
%% @doc reconfig_game/2
%% API for updating a game
%%
%% @spec reconfig_game(string(), {integer(), #game{}}) -> #game{}
%% @end
%%-------------------------------------------------------------------
reconfig_game(SessionId, Data={_GameId, _PropList}) ->
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
join_game(SessionId, Data={_GameId, _Country}) ->
    ?SESSION_CALL(SessionId, join_game, Data).
