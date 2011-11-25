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
%%% @doc Driver for the message flow - online version
%%%
%%% This module is to be run by basho bench
%%%
%%% @since : 22 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(tt_message_online).

-export([new/1, run/4]).

-include("basho_bench.hrl").

-record(state, {from, to}).

%%-------------------------------------------------------------------
%% @doc
%% Initialization
%% * Setup 2 users
%% @end
%%-------------------------------------------------------------------
new(_Id) ->
    Node = basho_bench_config:get(tt_node),
    load_test:connect(Node, 30),
    % Create 2 users
    Users = load_test:register_user(2),
    UserData = load_test:login(Users),
    Data = get_user_data(Users, UserData),
    {ok, #state{from=lists:nth(1, Data), to=lists:nth(2, Data)}}.

%%-------------------------------------------------------------------
%% @doc
%% Run
%% * Send message from one user to the other
%% * Check for message reception
%% @end
%%-------------------------------------------------------------------
run(test, _KeyGen, _ValueGen, State) ->
    {_FromNick, FromSessionId, _FromPid} = State#state.from,
    {ToNick, _ToSessionId, ToPid} = State#state.to,
    load_test:send_msg(FromSessionId, ToNick),
    case load_test:user_message_receiver(ToPid) of
        {ok, _} ->
            {ok, State};
        {error, Error} ->
            {error, Error, State}
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
get_user_data(Users, UserData) ->
    get_user_data(Users, UserData, []).

get_user_data([], [], Data) ->
    Data;
get_user_data([{Nick, _Password}|Users], [{SessionId, Pid}|UserData], Data) ->
    get_user_data(Users, UserData, [{Nick, SessionId, Pid}|Data]).
