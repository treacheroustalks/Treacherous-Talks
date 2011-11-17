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

-include_lib("datatypes/include/push_receiver.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").

%% ------------------------------------------------------------------
%% Internal API Function Exports
%% ------------------------------------------------------------------
-export([start/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% server state
-define(TIMEOUT, 108000000). % 30 minutes = 1000 * 60 * 60 * 30
-record(state, {user, session_id, history, push_receiver}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Starts a new gen_server and links it to its parent
%%
%% @spec start(#user{}, #session_history{}, #push_receiver{}) ->
%%         {ok, #state{}}
%% @end
%%-------------------------------------------------------------------
start(User=#user{},
      History,
      PushReceiver = #push_receiver{}) ->
    gen_server:start(?MODULE, [User, History, PushReceiver], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Initiates the controller_app_worker
%%
%% @spec init([any()]) -> {ok, #state{}}
%% @end
%%-------------------------------------------------------------------
%-spec init([#user{}, #sesion_history{}, #push_receiver{}]) -> {ok, #state{}}.
init([User, History, PushReceiver]) ->
    Id = session_id:from_pid(self()),
    session_presence:add(User#user.id, Id),
    {ok, #state{user = User,
                session_id = Id,
                history = session_history:add(History, Id),
                push_receiver = PushReceiver},
     ?TIMEOUT}.

%%-------------------------------------------------------------------
%% @doc
%% Handles call for game order
%% @end
%% [@spec handle_call({game_order::atom(), {GameId::string(), {list(), list()}},
%%                     From::{pid(), Tag}, #state{}) -> {reply,Reply,#state{}}.]
%%
%% succeed return {ok, GameOrderList}
%% fail return {error, {"some error exists", ErrorList}}
%% @end
%%-------------------------------------------------------------------
handle_call({game_order, {GameId, GameOrderList}}, _From,
                                                 State = #state{user=User}) ->
    Reply = game:put_game_order(GameId, User#user.id, GameOrderList),
    {reply, Reply, State, ?TIMEOUT};

%%-------------------------------------------------------------------
%% @doc
%% Handles call for off game user messages
%% @end
%% [@spec handle_call({user_msg::atom(), #frontend_msg{},
%%                     From::{pid(), Tag}, #state{}) -> {reply,Reply,#state{}}.]
%%
%% succeed return {ok, messageID :: integer()}
%% fail return :
%%           {error, nick_not_unique} |
%%           {error, invalid_nick}|
%%           {error, Error :: any()}
%% @end
%%-------------------------------------------------------------------
handle_call({user_msg, FEMsg = #frontend_msg{}}, _From,
                                                State = #state{user=User}) ->
    Message = #message{from_id = User#user.id,
                       from_nick = User#user.nick,
                       to_nick = FEMsg#frontend_msg.to,
                       content = FEMsg#frontend_msg.content},
    Reply = message:user_msg(Message),
    {reply, Reply, State};

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
    User1 = proplist:update_record(User, PropList),
    User2 = User1#user{last_session=Id},
    Reply = case user_management:update(User2) of
                {error, Error} ->
                    {error, Error};
                {ok, UpdatedUser} ->
                    {ok, UpdatedUser}
            end,
    {reply, Reply, State#state{user=User2}, ?TIMEOUT};
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
    {reply, {ok, GameId}, State, ?TIMEOUT};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for getting a game
%% @end
%% [@spec handle_call({get_game::GameId::Integer()},
%%                     From::{pid(), Tag}, #state{}) -> {reply, ok, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({get_game, GameId}, _From, State) ->
    Reply = case game:get_game(GameId) of
                {ok, Game} when is_record(Game, game) ->
                    {ok, Game};
                _ ->
                    {error, game_does_not_exist}
    end,
    {reply, Reply, State, ?TIMEOUT};
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
                            NewGame = proplist:update_record(Game, PropList),
                            NewGame2 = NewGame#game{last_session = Id},
                            game:reconfig_game(NewGame2)
                    end;
                _ ->
                    {error, game_does_not_exist}
    end,
    {reply, Reply, State, ?TIMEOUT};
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
    Overview = game:get_game_overview(GameId, User#user.id),
    {reply, Overview, State, ?TIMEOUT};
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
    {reply, Reply, State, ?TIMEOUT};

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
    {reply, Reply, State, ?TIMEOUT};

handle_call(Request, _From, State) ->
    io:format("Received unhandled call: ~p~n", [{Request, _From, State}]),
    {noreply, ok, State, ?TIMEOUT}.


handle_cast(Event = #push_event{}, State = #state{push_receiver = Receiver}) ->
    Pid = Receiver#push_receiver.pid,
    Args = Receiver#push_receiver.args,
    Type = Receiver#push_receiver.type,
    catch fe_push:send(Type, Args, Pid, Event),
    {noreply, State};

handle_cast(stop, State) ->
    stop(State),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    stop(State),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, State) ->
    stop(State),
    io:format(user, "[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
stop(State) ->
    User = State#state.user,
    UserId = User#user.id,
    session_presence:remove(UserId).
