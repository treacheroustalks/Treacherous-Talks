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
%%% @author Erik Timan <erol7391@student.uu.se>
%%%
%%% @doc The system manager application handles configuration of the Treacherous
%%% Talks system. It is intended to be the single point of entry for configuring
%%% and controlling the whole system on a server.
%%%
%%% @end
%%%
%%% @since : 23 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(system_manager).

-behaviour(gen_server).

-include("include/sysconfig.hrl").

%% Set a timeout for API calls.
-define(TIMEOUT, 30000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, update_config/1, start_release/1, stop_release/1,
        ping_release/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% @doc
%% Update all the config files needed to fulfill the given host configuration
%% tuple.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_config(hostconf()) -> ok | {error, term()}.
update_config(HostConfig) ->
    gen_server:call(?MODULE, {update_config, HostConfig}, ?TIMEOUT).

%% ------------------------------------------------------------------
%% @doc
%% Starts a given release and returns as soon as it is possible to ping the
%% VM. Please notice that this does not ensure that any applications has
%% started correctly.
%%
%% @end
%% ------------------------------------------------------------------
-spec start_release(relname()) -> ok | {error, term()}.
start_release(Relname) ->
    gen_server:call(?MODULE, {start_release, Relname}, ?TIMEOUT).

%% ------------------------------------------------------------------
%% @doc
%% Stops a given release and returns only when the release has been truly
%% stopped (VM is shutdown).
%%
%% @end
%% ------------------------------------------------------------------
-spec stop_release(relname()) -> ok | {error, term()}.
stop_release(Relname) ->
    gen_server:call(?MODULE, {stop_release, Relname}, ?TIMEOUT).

%% ------------------------------------------------------------------
%% @doc
%% Pings a given release and returns the result.
%%
%% @end
%% ------------------------------------------------------------------
-spec ping_release(relname()) -> up | down | {error, term()}.
ping_release(Relname) ->
    gen_server:call(?MODULE, {ping_release, Relname}, ?TIMEOUT).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call({update_config, HostConfig}, _From, State) ->
    {reply, int_update_config(HostConfig), State};
handle_call({start_release, Relname}, _From, State) ->
    {reply, int_start_release(Relname), State};
handle_call({stop_release, Relname}, _From, State) ->
    {reply, int_stop_release(Relname), State};
handle_call({ping_release, Relname}, _From, State) ->
    {reply, handle_releases:ping_release(Relname), State}.

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
%% Internal Functions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc
%% Applies a given host configuration tuple to local releases.
%%
%% @end
%% ------------------------------------------------------------------
-spec int_update_config(hostconf()) -> ok | {error, term()}.
int_update_config(HostConfig) ->
    {ok, Releasepath} = application:get_env(release_path),
    case HostConfig of
        {host, Hostname, RelConfs} ->
            update_app_config_loop(RelConfs, Hostname, Releasepath)
    end.

%% ------------------------------------------------------------------
%% @doc
%% Iterates over given release configurations and applies them to each release
%% in turn.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_app_config_loop([relconf()], hostname(), string()) ->
    ok | {error, term()}.
update_app_config_loop([], _Hostname, _Releasepath) ->
    ok;
update_app_config_loop([Config | T], Hostname, Releasepath) ->
    case Config of
        {release, Relname, RelConf} ->
            ok = update_app_config(Releasepath, Relname, RelConf),
            ok = update_node_name(Releasepath, Hostname, Relname),
            % Call ourself again
            update_app_config_loop(T, Hostname, Releasepath);
        _ ->
            {error, badconfig}
    end.

%% ------------------------------------------------------------------
%% @doc
%% Merges the given configuration with the configuration already present in the
%% app.config file in a release and writes it to app-override.config.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_app_config(string(), relname(), relconf()) ->
    ok | {error, term()}.
update_app_config(Releasepath, Relname, RelConf) ->
    Path = filename:join([Releasepath, Relname, "etc"]),
    % Get old app.config, merge with new and write to app-override.config
    {ok, OldConfig} = manage_config:read_config(Path++"/app.config"),
    NewConfig = manage_config:update_config(OldConfig, RelConf),
    manage_config:write_config(Path++"/app-override.config", NewConfig).

%% ------------------------------------------------------------------
%% @doc
%% Updates the nodename file containing the hostname for a given release.
%%
%% @end
%% ------------------------------------------------------------------
-spec update_node_name(string(), hostname(), relname()) ->
    ok | {error, term()}.
update_node_name(Releasepath, Hostname, Relname) ->
    Filename = filename:join([Releasepath, Relname, "etc", "nodename"]),
    Namestring = "-name "++atom_to_list(Relname)++"@"++Hostname,
    file:write_file(Filename, Namestring).

%% ------------------------------------------------------------------
%% @doc
%% Starts a given release and pings it to ensure that it is started. If the
%% release already was started, we return an error.
%%
%% @end
%% ------------------------------------------------------------------
-spec int_start_release(relname()) -> ok | {error, term()}.
int_start_release(Relname) ->
    case handle_releases:ping_release(Relname) of
        up ->
            % Crap, it is already started.
            {error, release_is_already_started};
        down ->
            % Ok, do regular start and try to ping it up to 15 times
            case handle_releases:start_release(Relname) of
                {ok, _Text} ->
                    case handle_releases:ping_release_and_wait(Relname, 15) of
                        up ->
                            ok;
                        down ->
                            {error, release_ping_timed_out};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% @doc
%% Stops a given release. If it is already stopped, this function will still
%% return ok.
%%
%% @end
%% ------------------------------------------------------------------
-spec int_stop_release(relname()) -> ok | {error, term()}.
int_stop_release(Relname) ->
    case handle_releases:ping_release(Relname) of
        up ->
            case handle_releases:stop_release(Relname) of
                {ok, _Text} ->
                    ok;
                Error ->
                    Error
            end;
        down ->
            ok;
        Error ->
            Error
    end.
