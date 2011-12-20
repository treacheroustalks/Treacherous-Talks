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
-module(release_status_tracker).
-behaviour(gen_server).

-include_lib("datatypes/include/clusterconf.hrl").
-include_lib ("utils/include/debug.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export ([start_link/0, ping/0, get_started_releases/0,
          get_started_releases_and_stop/0, save_status_of_release/2]).

%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% system_manager_config state
-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ping() ->
    gen_server:call(?MODULE, ping).

%% ------------------------------------------------------------------
%% @doc
%% Gets a list of started releases.
%%
%% @end
%% ------------------------------------------------------------------
-spec get_started_releases() -> {ok, list()} | {error, term()}.
get_started_releases() ->
    gen_server:call(?MODULE, {get_started_releases}).

%% ------------------------------------------------------------------
%% @doc
%% Gets a list of started releases and sets their status to 'stopped' at them
%% same time.
%%
%% @end
%% ------------------------------------------------------------------
-spec get_started_releases_and_stop() -> {ok, list()} | {error, term()}.
get_started_releases_and_stop() ->
    gen_server:call(?MODULE, {get_started_releases_and_stop}).

%% ------------------------------------------------------------------
%% @doc
%% Saves the current status of a release.
%%
%% @end
%% ------------------------------------------------------------------
-spec save_status_of_release(relname(), atom()) ->
    ok | {error, term()}.
save_status_of_release(Relname, Status) ->
    gen_server:call(?MODULE, {save_status_of_release, Relname, Status}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    ?DEBUG("[~p] starting ~p~n", [?MODULE, self()]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call({get_started_releases}, _From, State) ->
    Reply = get_all(),
    {reply, Reply, State};
handle_call({get_started_releases_and_stop}, _From, State) ->
    Reply = get_all_and_stop(),
    {reply, Reply, State};
handle_call({save_status_of_release, Relname, Status}, _From, State) ->
    Reply = save(Relname, Status),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    ?DEBUG("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    ?DEBUG("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

% This is breaking our abstractions, but it is the only simple way to do
% this. This should be done by a system_manager_worker, but since we cannot
% easily control their supervisor timeout setting, we're using an exported
% internal function there to be able to wait long enough for releases to stop.
terminate(shutdown, _State) ->
    {ok, Releases} = get_all_and_stop(),
    ?DEBUG("Shutting down the following releases: ~p~n", [Releases]),
    [ system_manager_worker:int_stop_release(Release) || Release <- Releases ],
    ok;
terminate(_Reason, _State) ->
    ?DEBUG("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc
%% Gets a list of started releases by reading and parsing the file
%% status-of-releases.
%%
%% @end
%% ------------------------------------------------------------------
-spec get_all() -> {ok, list()} | {error, term()}.
get_all() ->
    Filename = filename:join(["log", "status-of-releases"]),
    case manage_config:read_config(Filename) of
        {ok, List} ->
            % Get a list of the started releases by going through the list and
            % matching on started ones.
            {ok, [Release || {Release, started} <- List]};
        {error, enoent} ->
            {ok, []};
        Other ->
            Other
    end.

%% ------------------------------------------------------------------
%% @doc

%% Gets a list of started releases and sets the status of each to stopped before
%% returning the list. The intention of this function is to atomically update
%% the information so that subsequent calls will show that there are no started
%% releases. This is useful to ensure that only caller actually will try to stop
%% the releases.
%%
%% @end
%% ------------------------------------------------------------------
-spec get_all_and_stop() -> {ok, list()}.
get_all_and_stop() ->
    Status = {ok, Releases} = get_all(),
    [ save(Release, stopped) || Release <- Releases ],
    Status.

%% ------------------------------------------------------------------
%% @doc
%% Saves the current status of a release to the file status-of-releases in the
%% log directory. It updates the information in the file as needed.
%%
%% @end
%% ------------------------------------------------------------------
-spec save(relname(), atom()) -> ok | {error, term()}.
save(Relname, Status) ->
    Filename = filename:join(["log", "status-of-releases"]),
    ?DEBUG("Saving status ~p of release ~p.~n", [Status, Relname]),
    % See if we can read the old status
    case manage_config:read_config(Filename) of
        {ok, OldList} ->
            NewList = update_rel_status(OldList, [{Relname, Status}]),
            manage_config:write_config(Filename, NewList);
        {error, enoent} ->
            % No old file, nothing to merge. Just write to a new one.
            manage_config:write_config(Filename, [{Relname, Status}]);
        Other ->
            Other
    end.

%% ------------------------------------------------------------------
%% @doc
%%  Given a list of tuples [{relname, status}] and a list of changes,
%%  do a lists:keystore on each change so that existing releases
%%  are replaced, and new releases are insterted.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_rel_status([{atom(), atom()}], [{atom(), atom()}]) -> [{atom(), atom()}].
update_rel_status(OldStatuses, StatusChanges) ->
    Fun = fun({RelName, Change}, Config) ->
                  lists:keystore(RelName, 1, Config, {RelName, Change})
          end,
    lists:foldl(Fun, OldStatuses, StatusChanges).
