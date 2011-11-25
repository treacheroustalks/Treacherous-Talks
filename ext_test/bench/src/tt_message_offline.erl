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
%%% @doc Driver for the message flow - offline version
%%%
%%% This module is to be run by basho bench
%%%
%%% @since : 22 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(tt_message_offline).

-export([new/1, run/4]).

-include("basho_bench.hrl").

-record(state, {from, to}).

%%-------------------------------------------------------------------
%% @doc
%% Initialization
%% * Setup 2 users
%% * Send message from one user to the other
%% @end
%%-------------------------------------------------------------------
new(_Id) ->
    Node = basho_bench_config:get(tt_node),
    load_test:connect(Node, 30),
    % Create 2 users
    {FromNick, FromPass} = load_test:register_user(),
    {ToNick, ToPass} = load_test:register_user(),
    % Login from user
    {FromSessionId, FromPid} = load_test:login(FromNick, FromPass),
    State = #state{to={ToNick, ToPass}, from={FromSessionId, FromPid}},
    {ok, State}.

%%-------------------------------------------------------------------
%% @doc
%% Run
%% * Login second user
%% * Read messages
%% @end
%%-------------------------------------------------------------------
run(test, _KeyGen, _ValueGen, State) ->
    {FromSessionId, _FromPid} = State#state.from,
    {ToNick, ToPass} = State#state.to,

    % Send message
    load_test:send_msg(FromSessionId, ToNick),

    % Login second user
    {ToSessionId, ToPid} = load_test:login(ToNick, ToPass),

    % Check if message is received
    case load_test:user_message_receiver(ToPid) of
        {ok, _} ->
            % logout the second user
            load_test:logout(ToSessionId, ToPid),
            {ok, State};
        {error, Error} ->
            load_test:logout(ToSessionId, ToPid),
            {error, Error, State}
    end.
