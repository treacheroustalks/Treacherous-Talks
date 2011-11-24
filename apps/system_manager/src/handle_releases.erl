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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_release/1, stop_release/1, ping_release/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Starts a release given the name of it. This assumes that the start script for
%% a release is named like "Name/bin/Name". It will also return the stdout of
%% the command as a binary.
%%
%% @end
%%-------------------------------------------------------------------
-spec start_release(string()) ->
    {ok, binary()} | {error, any()}.
start_release(Name) ->
    run_release_script(Name, "start").

%%-------------------------------------------------------------------
%% @doc
%% Stops a release given the name of it. This assumes that the start script for
%% a release is named like "Name/bin/Name". It will also return the stdout of
%% the command as a binary.
%%
%% @end
%%-------------------------------------------------------------------
-spec stop_release(string()) ->
    {ok, binary()} | {error, any()}.
stop_release(Name) ->
    run_release_script(Name, "stop").

%%-------------------------------------------------------------------
%% @doc
%% Pings a release given the name of it and returns if it is up or down. This
%% assumes that the start script for a release is named like "Name/bin/Name".
%%
%% @end
%%-------------------------------------------------------------------
-spec ping_release(string()) ->
    up | down.
ping_release(Name) ->
    case run_release_script(Name, "ping") of
        {ok, _} ->
            up;
        {error, _} ->
            down
    end.

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
run_release_script(Name, Action) ->
    Releasepath = application:get_env(release_path),
    Filename = filename:join([Releasepath, Name, "bin", Name]),
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
