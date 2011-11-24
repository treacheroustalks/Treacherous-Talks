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
%%% @doc This module handles Erlang releases, that is starting, stopping and
%%% pinging them through low-level means. It basically executes the control
%%% scripts directly. It is assumed that the release_path environment variable
%%% is set.
%%%
%%% @end
%%%
%%% @since : 23 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(handle_releases).

-include_lib("datatypes/include/clusterconf.hrl").
-include_lib("utils/include/debug.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_release/1,
         stop_release/1,
         ping_release/1,
         ping_release_and_wait/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Starts a release given the name of it. This assumes that the start script for
%% a release is named like "Relname/bin/Relname". It will also return the stdout
%% of the command as a binary.
%%
%% @end
%%-------------------------------------------------------------------
-spec start_release(relname()) -> {ok, binary()} | {error, term()}.
start_release(Relname) ->
    run_release_script(Relname, "start").

%%-------------------------------------------------------------------
%% @doc
%% Stops a release given the name of it. This assumes that the start script for
%% a release is named like "Relname/bin/Relname". It will also return the stdout
%% of the command as a binary.
%%
%% @end
%%-------------------------------------------------------------------
-spec stop_release(relname()) -> {ok, binary()} | {error, term()}.
stop_release(Relname) ->
    run_release_script(Relname, "stop").

%%-------------------------------------------------------------------
%% @doc
%% Pings a release given the name of it and returns if it is up or down. This
%% assumes that the start script for a release is named like
%% "Relname/bin/Relname".
%%
%% @end
%%-------------------------------------------------------------------
-spec ping_release(relname()) -> up | down | {error, term()}.
ping_release(Relname) ->
    case run_release_script(Relname, "ping") of
        {ok, _} ->
            up;
        {error, {1, _}} ->
            down;
        Error ->
            Error
    end.

%%-------------------------------------------------------------------
%% @doc
%% Pings a given release and checks for an "up" reply. If the reply is "down",
%% it will sleep one second and ping again. This is repeated either until an
%% "up" reply is received or when it reaches the NbrOfTimes limit.
%%
%% @end
%%-------------------------------------------------------------------
-spec ping_release_and_wait(relname(), integer()) ->
    up | down | {error, term()}.
ping_release_and_wait(Relname, NbrOfTimes) when NbrOfTimes > 1 ->
    case ping_release(Relname) of
        down ->
            timer:sleep(1000),
            ping_release_and_wait(Relname, NbrOfTimes-1);
        Other ->
            Other
    end;
ping_release_and_wait(Relname, _NbrOfTimes) ->
    ping_release(Relname).

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Creates a command from the name of a release and the action to perform and
%% runs that command.
%%
%% @end
%%-------------------------------------------------------------------
run_release_script(Relname, Action) ->
    {ok, Releasepath} = application:get_env(release_path),
    % It is important to do absname since os:find_executable is of another
    % opinion what a relative path is relative to than the filename lib.
    Filename = filename:absname(
                 filename:join([Releasepath, Relname, "bin", Relname])),
    ?DEBUG("Doing ~p on file ~p~n", [Action, Filename]),
    cmd(Filename, [Action]).

% The following three functions are copied straight off the net since Erlang
% doesn't have a inbuilt way to return exit codes when running an external
% command, nor good escaping of arguments. Some small modifications were done to
% return the exit code. Source: http://stackoverflow.com/questions/2231061/
cmd(Cmd, Args) ->
    case os:find_executable(Cmd) of
        false ->
            {error, nosuchfile};
        _FullPath ->
            Tag = make_ref(),
            {Pid, Ref} = erlang:spawn_monitor(fun() ->
                                Rv = cmd_sync(Cmd, Args),
                                exit({Tag, Rv})
                            end),
            receive
                {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
                {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
            end
    end.

cmd_sync(Cmd, Args) ->
    P = open_port({spawn_executable, os:find_executable(Cmd)}, [
            binary, use_stdio, stream, exit_status, {args, Args}]),
    cmd_receive(P, []).

cmd_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} -> cmd_receive(Port, [Data|Acc]);
        {Port, {exit_status, 0}} -> {ok, lists:reverse(Acc)};
        {Port, {exit_status, S}} -> {error, {S, lists:reverse(Acc)}}
    end.
