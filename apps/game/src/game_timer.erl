-module(game_timer).
-behaviour(gen_fsm).
-include_lib("datatypes/include/game.hrl").
%% ------------------------------------------------------------------
%% Ecternal API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, event/2, current_state/1]).

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

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Game) ->
    gen_fsm:start_link(?MODULE, Game, []).

%%-------------------------------------------------------------------
%% @doc
%% Sends an event to change the state of the FSM Timer.
%% @end
%% [@spec create(Timer::pid(), Event::atom()) -> ok.
%% @end]
%%-------------------------------------------------------------------
-spec event(pid(), atom()) -> ok.
event(Timer, Event) ->
    gen_fsm:send_event(Timer, Event).

%%-------------------------------------------------------------------
%% @doc
%% Sends an event to Timer, to find out the current state
%% @end
%% [@spec create(Timer::pid()) -> StateName::atom().
%% @end]
%%-------------------------------------------------------------------
-spec current_state(pid()) -> atom().
current_state(Timer) ->
    gen_fsm:sync_send_all_state_event(Timer, statename).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Game) ->
    service_worker:join_group(?MODULE),
    {ok, waiting_phase, #state{game = Game, phase = waiting_phase}}.

waiting_phase(_Event, State) ->
    {next_state, order_phase, State#state{phase = order_phase}}.
% TODO: Currently "restarts" the phase
waiting_phase(_Event, _From, State) ->
    Reply = {waiting_phase, State},
    {reply, Reply, waiting_phase, State}.


order_phase(_Event, State) ->
    {next_state, retreat_phase, State#state{phase = retreat_phase}}.
% TODO: Currently "restarts" the phase
order_phase(_Event, _From, State) ->
    Reply = {order_phase, State},
    {reply, Reply, order_phase, State}.


retreat_phase(_Event, State) ->
    {next_state, build_phase, State#state{phase = retreat_phase}}.
% TODO: Currently "restarts" the phase
retreat_phase(_Event, _From, State) ->
    Reply = {retreat_phase, State},
    {reply, Reply, retreat_phase, State}.


build_phase(_Event, State) ->
    {next_state, order_phase, State#state{phase = build_phase}}.
% TODO: Currently "restarts" the phase
build_phase(_Event, _From, State) ->
    Reply = {build_state, State},
    {reply, Reply, build_phase, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(statename, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    io:format("Terminating game timer ~p~n", [(State#state.game)#game.id]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

