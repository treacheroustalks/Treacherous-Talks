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
-module(game_timer).
-behaviour(gen_fsm).

-include_lib("utils/include/debug.hrl").

-include_lib ("datatypes/include/bucket.hrl").
-include_lib("datatypes/include/game.hrl").
-include_lib ("eunit/include/eunit.hrl").
%% ------------------------------------------------------------------
%% External API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, event/2, sync_event/2,
         current_state/1, get_game_state/1,
         stop/1, stop/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([waiting_phase/2, waiting_phase/3,
        order_phase/2, order_phase/3, retreat_phase/2,
        retreat_phase/3, build_phase/2, build_phase/3]).

%%---------------------------------------------------------------------
%% Datatype: state
%% where:
%%       game: a game record
%%       phase: the current state
%%---------------------------------------------------------------------
-record(state, {game, phase = init}).

%% define start year
-define(START_YEAR, 1901).
%% define rules module
-define(RULES, diplomacy_rules).
%% defines the game id
-define(ID(State),(State#state.game)#game.id).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Game) ->
    gen_fsm:start_link({global, {?MODULE, Game#game.id}}, ?MODULE, Game, []).

%%-------------------------------------------------------------------
%% @doc
%% Sends an event to change the state of the FSM Timer.
%% @spec event(Timer::pid(), Event::atom()) -> ok
%% @end
%%-------------------------------------------------------------------
-spec event(pid(), atom()) -> ok.
event(Timer, Event) ->
    gen_fsm:send_event({global, {?MODULE, Timer}}, Event).


%%-------------------------------------------------------------------
%% @doc
%% Sends a synchronous event to change the state of the FSM Timer.
%% @spec sync_event(Timer::pid(), Event::atom()) -> {ok, CurrentState :: atom()}
%% @end
%%-------------------------------------------------------------------
sync_event(Timer, Event) ->
    gen_fsm:sync_send_event({global, {?MODULE, Timer}}, Event).

%%-------------------------------------------------------------------
%% @doc
%% Sends an event to Timer, to find out the current state (ONLY for testing)
%% @spec current_state(Timer::pid()) -> StateName::atom()
%% @end
%%-------------------------------------------------------------------
-spec current_state(pid()) -> atom().
current_state(Timer) ->
    gen_fsm:sync_send_all_state_event({global, {?MODULE, Timer}}, phasename).
%%-------------------------------------------------------------------
%% @doc
%% Sends an event to Timer, to find out the state of the game (ONLY for testing)
%% @spec get_game_state(Timer :: pid()) -> Game :: #game{}
%% @end
%%-------------------------------------------------------------------
-spec get_game_state(pid()) -> atom().
get_game_state(Timer) ->
    gen_fsm:sync_send_all_state_event({global, {?MODULE, Timer}}, game).

%%-------------------------------------------------------------------
%% @doc
%% Sends an event to Timer, to stop it
%% @spec stop(Timer::pid()) -> ok
%% @end
%%-------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Timer) ->
    gen_fsm:sync_send_all_state_event({global, {?MODULE, Timer}}, stop).
%%-------------------------------------------------------------------
%% @doc
%% Sends an event to Timer, to stop it and update the game to be either
%% finished or stopped
%% @spec stop(Timer::pid(), NewState :: finished | stopped) -> ok
%% @end
%%-------------------------------------------------------------------
-spec stop(pid(), finished | stopped) -> ok.
stop(Timer, NewState) ->
    gen_fsm:sync_send_all_state_event({global, {?MODULE, Timer}},
                                      {stop, NewState}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Game) ->
    Timeout = timer:minutes(Game#game.waiting_time),
    {ok, waiting_phase, #state{game = Game, phase = waiting_phase}, Timeout}.


waiting_phase({reconfig, UpdatedGame}, State) ->
    Timeout = timer:minutes(UpdatedGame#game.waiting_time),
    {next_state, waiting_phase, State#state{game = UpdatedGame}, Timeout};
waiting_phase(timeout, State) ->
    %% This is where we "move" on to the next state!
    NewState = State#state{phase = order_phase,
                           game = State#state.game#game{status = ongoing}},
    phase_change(NewState#state.game, started),
    Timeout = timer:minutes((State#state.game)#game.order_phase),
    {next_state, order_phase, NewState, Timeout};
waiting_phase(_Event, State) ->
    %% any other event than timeout or reconfig
    Timeout = timer:minutes((State#state.game)#game.waiting_time),
    {next_state, waiting_phase, State, Timeout}.
waiting_phase(_Event, From, State) ->
    ?debugVal(syncevent(waiting_phase, From, State)).


order_phase(_Event, State) ->
    process_phase(?ID(State), order_phase),
    phase_change(?ID(State), retreat_phase),
    Timeout = timer:minutes((State#state.game)#game.retreat_phase),
    {next_state, retreat_phase, State#state{phase = retreat_phase}, Timeout}.
order_phase(_Event, From, State) ->
    syncevent(order_phase, From, State).


retreat_phase(_Event, State) ->
    process_phase(?ID(State), retreat_phase),
    %% retreat is handled and we enter count phase
    case phase_change(?ID(State), build_phase) of
        {ok, true} ->
            Timeout = timer:minutes((State#state.game)#game.build_phase),
            {next_state, build_phase, State#state{phase = build_phase}, Timeout};
        {ok, skip} ->
            phase_change(?ID(State), order_phase),
            Timeout = timer:minutes((State#state.game)#game.order_phase),
            {next_state, order_phase, State#state{phase = order_phase}, Timeout};
        {stop, Reason, game_over} ->
            {stop, Reason, State}
    end.
retreat_phase(_Event, From, State) ->
    syncevent(retreat_phase, From, State).


build_phase(_Event, State) ->
    process_phase(?ID(State), build_phase),
    phase_change(?ID(State), order_phase),
    Timeout = timer:minutes((State#state.game)#game.order_phase),
    {next_state, order_phase, State#state{phase = order_phase}, Timeout}.
build_phase(_Event, From, State) ->
    syncevent(build_phase, From, State).


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({stop, NewState}, _From, _StateName, State) ->
    ?DEBUG("Stopping timer for ~p...~n", [(State#state.game)#game.id]),
    end_game((State#state.game)#game.id, NewState),
    {stop, normal, {(State#state.game)#game.id, NewState}, State};
handle_sync_event(stop, _From, _StateName, State) ->
    ?DEBUG("Stopping timer for ~p...~n", [(State#state.game)#game.id]),
    {stop, normal, stop_request, State};
handle_sync_event(statename, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(phasename, _From, StateName, State) ->
    {reply, State#state.phase, StateName, State};
handle_sync_event(game, _From, StateName, State) ->
    {reply, State#state.game, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    ?DEBUG("Terminating game timer ~p~n", [State]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Handles the change of a phase; updates the current game and its state
%% valid argument pairs are (#game{}, started) or (ID, Phase)
%% @spec
%% phase_change(Game :: #game{} | integer,  Phase :: atom()) ->
%%    Result :: any()
%% @end
%%-------------------------------------------------------------------
phase_change(Game = #game{id = ID}, started) ->
    ?DEBUG("Game ~p started~n", [ID]),
    %% maybe tell the users about game start?
    game_worker:update_game(ID, Game),
    setup_game(ID),
    game_join_proc:stop(ID);
phase_change(ID, build_phase) ->
    ?DEBUG("Game ~p entered build_phase~n", [ID]),
    {ok, GameState} = game_utils:get_game_state(ID),
    % skip count phase if it is not fall
    case GameState#game_state.year_season of
        {_Year, spring} -> {ok, skip};
        _Other ->
            Map = game_utils:to_rule_map(GameState#game_state.map),
            Results = rules:process(count_phase, Map, ?RULES, []),
            game_utils:delete_map(Map),
            case ?debugVal(lists:member (game_over, Results)) of
                false ->
                    % inform players of their possible builds?
                    NewCurrentGame = update_current_game(ID, build_phase),
                    new_state(NewCurrentGame, GameState#game_state.map),
                    {ok, true};
                true ->
                    % game over!
                    ?DEBUG("Game over ~p~n", [ID]),
                    end_game(ID, finished),
                    {stop, normal, game_over}
            end
    end;
phase_change(ID, Phase) ->
    ?DEBUG("Game ~p entered ~p~n", [ID, Phase]),
    {ok, OldState} = game_utils:get_game_state(ID),
    NewCurrentGame = update_current_game(ID, Phase),
    new_state(NewCurrentGame, OldState#game_state.map).

%%-------------------------------------------------------------------
%% @doc
%% Processes the end of a phase, by calling the rule engine with the
%% orders the game has received.
%% @spec
%% process_phase(ID :: integer() , Phase :: atom()) ->
%%    {ok, Result :: any()}
%% @end
%%-------------------------------------------------------------------
process_phase(ID, Phase) ->
    {ok, GameState} = game_utils:get_game_state(ID),
    Map = game_utils:to_rule_map(GameState#game_state.map),
    ?debugVal(Orders = game_utils:get_all_orders(ID)),
    ?DEBUG("Received orders: ~p~n", [Orders]),
    Result = rules:process(Phase, Map, ?RULES, Orders),
    update_state(GameState#game_state{map = game_utils:to_mapterm(Map)}),
    game_utils:delete_map(Map),
    %% we probably want to handle the result in some way
    %% like informing the users
    {ok, Result}.

%%-------------------------------------------------------------------
%% @doc
%% Updates the current game record for the game with id ID, to represent
%% its current game; phase, year, season.
%% @spec
%% update_current_game(ID :: integer() , Phase :: atom()) ->
%%    NewCurrentGame :: #game_current{} | Error
%% @end
%%-------------------------------------------------------------------
update_current_game(ID, NewPhase) ->
    BinKey = game_utils:get_game_current_key(ID),
    DBReply = db:get(?B_GAME_CURRENT, BinKey),
    case DBReply of
        {ok, CurrentGameObj} ->
            OldGame = db_obj:get_value(CurrentGameObj),
            NewGame = case {NewPhase, OldGame#game_current.year_season} of
                          {order_phase, {Year, fall}} ->
                              OldGame#game_current{
                                year_season = {Year+1, spring},
                                current_phase = NewPhase};
                          {order_phase, {Year, spring}} ->
                              OldGame#game_current{
                                year_season = {Year, fall},
                                current_phase = NewPhase};
                          _OtherPhase ->
                              OldGame#game_current{
                                current_phase = NewPhase}
                      end,
            game_utils:update_db_obj(CurrentGameObj, NewGame);
        Other ->
            Other
    end.


%%-------------------------------------------------------------------
%% @doc
%% Updates the state record
%% @spec
%% update_state(NewState :: #game_state{}) ->
%%    {ok, NewState}
%% @end
%%-------------------------------------------------------------------
update_state(#game_state{id = ID} = NewState) ->
    Key = game_utils:get_keyprefix({id, ID}),
    {ok, OldStateObj} = db:get(?B_GAME_STATE, Key),
    game_utils:update_db_obj(OldStateObj, NewState).

%%-------------------------------------------------------------------
%% @doc
%% Creates a game state record and current game record for a game with id ID.
%% @spec
%% setup_game(ID :: integer()) -> ok
%% @end
%%-------------------------------------------------------------------
setup_game(ID) ->
    %% create initial game state
    Map = game_utils:get_default_map(),
    GameState = #game_state{id = ID,
                            phase = order_phase,
                            year_season = {?START_YEAR, spring},
                            map = game_utils:to_mapterm(Map)},
    StateKey = list_to_binary(integer_to_list(ID) ++
                              "-" ++ integer_to_list(?START_YEAR) ++
                              "-" ++ "spring" ++
                              "-" ++ "order_phase"),
    DBGameState = db_obj:create(?B_GAME_STATE, StateKey, GameState),
    %% Link the game state to its game
    GameStateLinkObj = db_obj:add_link(DBGameState,
                                       {{?B_GAME,
                                         db:int_to_bin(ID)},
                                        ?GAME_STATE_LINK_GAME}),
    db:put(GameStateLinkObj),
    %% create first current game
    CurrentGame = #game_current{id = ID,
                                year_season = {?START_YEAR, spring},
                                current_phase = order_phase},
    CurrentKey = game_utils:get_game_current_key(ID),
    DBCurrentGame = db_obj:create(?B_GAME_CURRENT, CurrentKey, CurrentGame),
    %% Link the current game to its gamestate
    CurrentGameLinkObj = db_obj:add_link(DBCurrentGame,
                                         {{?B_GAME_STATE, StateKey},
                                          ?CURRENT_GAME_LINK_STATE}),
    db:put(CurrentGameLinkObj),
    game_utils:delete_map(Map).


%%-------------------------------------------------------------------
%% @doc
%% Updates game record and stops the timer, when a game has finished.
%% NewStatus can either be the atom finished or stopped
%% @spec
%% end_game(GameID :: integer(), NewStatus :: atom()) -> any()
%% @end
%%-------------------------------------------------------------------
end_game(GameID, NewStatus) ->
    {ok, Game} = game:get_game(GameID),
    FinishedGame = Game#game{status = NewStatus},
    game_worker:update_game(GameID, FinishedGame).

%%-------------------------------------------------------------------
%% @doc
%% Creates a new game state record, with data from the current game
%% record and a map.
%% @spec
%% new_state(CurrentGame :: #game_current{}, Map :: map()) ->
%%    Result :: any()
%% @end
%%-------------------------------------------------------------------
new_state(CurrentGame, Map) ->
    ?debugVal(CurrentGame),
    Key = game_utils:get_keyprefix({game_current, CurrentGame}),
    GameState = #game_state{id = CurrentGame#game_current.id,
                            year_season = CurrentGame#game_current.year_season,
                            phase = CurrentGame#game_current.current_phase,
                            map = Map},
    DBGameState = db_obj:create(?B_GAME_STATE, Key, GameState),
    %% Link the game state to its game
    GameStateLinkObj = db_obj:add_link(DBGameState,
                                       {{?B_GAME,
                                         db:int_to_bin(CurrentGame#game_current.id)},
                                        ?GAME_STATE_LINK_GAME}),
    db:put(GameStateLinkObj).


%%-------------------------------------------------------------------
%% @doc
%% Synchronous calls are handled by syncevent, it will reply ok when
%% it has finished processing the old phase and changing to the new,
%% these are used for testing.
%% @end
%%-------------------------------------------------------------------
syncevent(waiting_phase, From, State) ->
    %% This is where we "move" on to the next state!
    NewState = State#state{phase = order_phase,
                           game = State#state.game#game{status = ongoing}},
    phase_change(NewState#state.game, started),
    Timeout = timer:minutes((State#state.game)#game.order_phase),
    gen_fsm:reply(From, {ok, order_phase}),
    {next_state, order_phase, NewState, Timeout};
syncevent(order_phase, From, State) ->
    ?debugMsg("Received order_phase sync event"),
    ?debugVal(process_phase(?ID(State), order_phase)),
    ?debugVal(phase_change(?ID(State), retreat_phase)),
    Timeout = timer:minutes((State#state.game)#game.retreat_phase),
    gen_fsm:reply(From, {ok, retreat_phase}),
    {next_state, retreat_phase, State#state{phase = retreat_phase}, Timeout};
syncevent(retreat_phase, From, State) ->
    ?debugMsg("Received retreat_phase sync event"),
    process_phase(?ID(State), retreat_phase),
    %% retreat is handled and we enter count phase
    case ?debugVal(phase_change(?ID(State), build_phase)) of
        {ok, true} ->
            Timeout = timer:minutes((State#state.game)#game.build_phase),
            gen_fsm:reply(From, {ok, build_phase}),
            {next_state, build_phase, State#state{phase = build_phase}, Timeout};
        {ok, skip} ->
            phase_change(?ID(State), order_phase),
            Timeout = timer:minutes((State#state.game)#game.order_phase),
            gen_fsm:reply(From, {ok, order_phase}),
            {next_state, order_phase, State#state{phase = order_phase}, Timeout};
        {stop, Reason, game_over} ->
            gen_fsm:reply(From, {ok, game_over}),
            {stop, Reason, State}
    end;
syncevent(build_phase, From, State) ->
    ?debugMsg("Received build_phase sync event"),
    process_phase(?ID(State), build_phase),
    phase_change(?ID(State), order_phase),
    Timeout = timer:minutes((State#state.game)#game.order_phase),
    gen_fsm:reply(From, {ok, order_phase}),
    {next_state, order_phase, State#state{phase = order_phase}, Timeout}.
