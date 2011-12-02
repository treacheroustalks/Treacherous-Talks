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
         ping_release_and_wait/2,
         run_riak_search_command/1,
         run_riak_join_command/1]).

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
    run_release_script(Relname, Relname, "start").

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
    run_release_script(Relname, Relname, "stop").

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
    case run_release_script(Relname, Relname, "ping") of
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

%%-------------------------------------------------------------------
%% @doc
%% Runs the Riak search-cmd file with arguments according to Args.
%%
%% @end
%%-------------------------------------------------------------------
-spec run_riak_search_command(list()) -> {ok, binary()} | {error, term()}.
run_riak_search_command(Args) ->
    Res = run_command_with_cmd(riak, "search-cmd", Args),
    % Yes, it is brittle to regexp on messages from Riaks' search-cmd, but it is
    % better than nothing.
    case re:run(Res, "hook on bucket|Updating schema for", []) of
        {match, _Captured} ->
            {ok, Res};
        _Other ->
            {error, Res}
    end.

%%-------------------------------------------------------------------
%% @doc
%% Joins a Riak node to a Riak cluster by sending a join request to Node.
%%
%% @end
%%-------------------------------------------------------------------
-spec run_riak_join_command(hostname()) ->
    {ok, binary()} | {error, term()}.
run_riak_join_command(Node) ->
    Args = "join "++Node,
    OKResponse = "Sent join request to "++Node++"\n",
    case run_command_with_cmd(riak, "riak-admin", Args) of
        OKResponse ->
            {ok, OKResponse};
        "Failed: This node is already a member of a cluster\n" ->
            {error, alreadymember};
        Other ->
            {error, Other}
    end.


%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Creates a command from the name of a release, the file to execute, the action
%% to perform and runs that command. This function uses os:cmd which does not
%% check the error code and just returnes the STDOUT.
%%
%% @end
%%-------------------------------------------------------------------
-spec run_command_with_cmd(relname(), string(), string()) -> binary().
run_command_with_cmd(Relname, Binname, Argstring) ->
    {ok, Releasepath} = application:get_env(release_path),
    % It is important to do absname since os:find_executable is of another
    % opinion what a relative path is relative to than the filename lib.
    Filename = filename:absname(
                 filename:join([Releasepath, Relname, "bin", Binname])),
    ?DEBUG("Doing ~p on file ~p~n", [Argstring, Filename]),
    list_to_binary(os:cmd(Filename++" "++Argstring)).

%%-------------------------------------------------------------------
%% @doc
%% Creates a command from the name of a release, the file to execute, the action
%% to perform and runs that command. This function uses cmd() which checks the
%% return code, but instead behaves like exec() instead of a shell.
%%
%% @end
%%-------------------------------------------------------------------
-spec run_release_script(relname(), string(), string()) ->
    {ok, binary()} | {error, term()}.
run_release_script(Relname, Binname, Action) ->
    {ok, Releasepath} = application:get_env(release_path),
    % It is important to do absname since os:find_executable is of another
    % opinion what a relative path is relative to than the filename lib.
    Filename = filename:absname(
                 filename:join([Releasepath, Relname, "bin", Binname])),
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
