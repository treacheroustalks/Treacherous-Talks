%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <andre.hilsendeger@gmail.com>
%%%
%%% @doc Session process. Handles all request of one user.
%%% @end
%%%
%%% @since : 02 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(session_proc).
-behaviour(gen_server).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%% ------------------------------------------------------------------
%% Internal API Function Exports
%% ------------------------------------------------------------------
-export([start/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% Exports for eUnit - do not use!
%% ------------------------------------------------------------------
-export([update_rec_by_proplist/2]).

%% server state
-record(state, {user, session_id}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Starts a new gen_server and links it to its parent
%% @end
%% [@spec start_link() -> {ok, #state{}}.
%% @end]
%%-------------------------------------------------------------------
-spec start(#user{}) -> {ok, #state{}}.
start(User=#user{}) ->
    gen_server:start(?MODULE, User, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Initiates the controller_app_worker
%% @end
%% [@spec init(User::#user{}) -> {ok, #state{}}.
%% @end]
%%-------------------------------------------------------------------
-spec init(#user{}) -> {ok, #state{}}.
init(User) ->
    Id = session_id:from_pid(self()),
    {ok, #state{user = User, session_id = Id}}.

%%-------------------------------------------------------------------
%% @doc
%% Handles call for updating a user
%% @end
%% [@spec handle_call({create_user::atom(), #user{}},
%%                     From::{pid(), Tag}, #state{}) -> {noreply, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({update_user, PropList}, _From,
            State = #state{session_id=Id, user=User}) ->
    User1 = update_rec_by_proplist(User, PropList),
    User2 = User1#user{last_session=Id},
    Reply = case user_management:update(User2) of
                {error, Error} ->
                    {error, Error};
                {ok, UpdatedUser} ->
                    {ok, UpdatedUser}
            end,
    {reply, Reply, State#state{user=User2}};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for creating a new game
%% @end
%% [@spec handle_call({create_game::atom(), #game{}},
%%                     From::{pid(), Tag}, #state{}) -> {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({create_game, Game}, _From,
            State=#state{user=User}) ->
    Creator = User#user.id,
    {ok, GameId} = game:new_game(Game#game{creator_id = Creator}),
    % @todo no invalid create_game case yet ?
    {reply, {ok, GameId}, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for updating a game
%% @end
%% [@spec handle_call({reconfig_game::atom(), #game{}},
%%                     From::{pid(), Tag}, #state{}) -> {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({reconfig_game, {GameId, PropList}}, _From,
            State=#state{session_id = Id, user=User}) ->
    UserId = User#user.id,
    Reply = case game:get_game(GameId) of
                {ok, Game} when is_record(Game, game) ->
                    if
                        Game#game.status /= waiting ->
                            {error, game_started_already};
                        Game#game.creator_id /= UserId ->
                            {error, not_game_creator};
                        true ->
                            NewGame = update_rec_by_proplist(Game, PropList),
                            NewGame2 = NewGame#game{last_session = Id},
                            game:reconfig_game(NewGame2)
                    end;
                _ ->
                    {error, game_does_not_exist}
    end,
    {reply, Reply, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for getting an overview of a game
%% @end
%% [@spec handle_call({game_overview::atom(), GameId::Integer(), UserId::Integer()},
%%                     From::{pid(), Tag}, #state{}) -> {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({game_overview, GameId}, _From,
            State = #state{user=User}) ->
    Overview = game:get_game_state(GameId, User#user.id),
    {reply, Overview, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for joining a game
%% @end
%% [@spec handle_call({join_game::atom(), GameId::Integer(), UserId::Integer(),
%%                     Country::country()}, From::{pid(), Tag}, #state{}) ->
%%                                                  {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({join_game, {GameId, Country}}, _From,
            State = #state{user=User}) ->
    Reply = game:join_game(GameId, User#user.id, Country),
    {reply, Reply, State};

%%-------------------------------------------------------------------
%% @doc
%% Handles call for getting the user of the session.
%% @end
%% [@spec handle_call({get_session_user::atom(), UserId::Integer(),
%%                     Country::country()}, From::{pid(), Tag}, #state{}) ->
%%                                                  {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call(get_session_user, _From,
            State = #state{user=User}) ->
    Reply = {ok, User},
    {reply, Reply, State};

% @todo get_session_user

handle_call(Request, _From, State) ->
    io:format("Received unhandled call: ~p~n", [{Request, _From, State}]),
    {noreply, ok, State}.


handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format(user, "[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc update record via a proplist
%%  Input: Arg1: OldUser#user
%%         Arg2: [{#user.name, "username"}, {#user.password, "xxxx"}]
%%
%%  Output: #user{name="username", password="xxxx"}
%% @end
%%------------------------------------------------------------------------------
update_rec_by_proplist(Old, [{_, field_missing}|Rest]) ->
    update_rec_by_proplist(Old, Rest);
update_rec_by_proplist(Old, [{Field, Value}|Rest]) ->
    update_rec_by_proplist(setelement(Field, Old, Value), Rest);
update_rec_by_proplist(Updated, []) ->
    Updated.

