-module(user_management_config).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export ([start_link/0, node_pids/0, node_count/0, worker_count/1,
          worker_count/2, ping/0]).

%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% user_management_config state
-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

ping() ->
    gen_server:call(select_pid(), ping).

node_pids() ->
    service_conf:node_pids(?MODULE).

node_count() ->
    service_conf:node_count(?MODULE).

worker_count(Pid) ->
    service_conf:worker_count(Pid).

worker_count(Pid, Count) ->
    service_conf:worker_count(Pid, Count).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(no_arg) ->
    io:format ("[~p] starting ~p~n", [?MODULE, self()]),
    join_group(),
    {ok, #state{}}.

handle_call(worker_count, _From, State) ->
    Count = user_management_worker_sup:worker_count(),
    {reply, Count, State};
handle_call({worker_count, Count}, _From, State) ->
    Response = user_management_worker_sup:worker_count(Count),
    {reply, Response, State};
handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call(_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format ("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

join_group() ->
    pg2:create(?MODULE),
    pg2:join(?MODULE, self()).

select_pid() ->
    pg2:get_closest_pid(?MODULE).
