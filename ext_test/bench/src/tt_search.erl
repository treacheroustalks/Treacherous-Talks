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
%%% @doc Driver for the search flow
%%%
%%% This module is to be run by basho bench
%%%
%%% @since : 22 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(tt_search).

-export([new/1, run/4]).

-include("basho_bench.hrl").

-record(state, {session}).

-define(GAME_COUNT, 10).

%%-------------------------------------------------------------------
%% @doc
%% Initialization
%% * Setup a user
%% * Create new game
%% @end
%%-------------------------------------------------------------------
new(_Id) ->
    Node = basho_bench_config:get(tt_node),
    pg2:start(),
    pong = net_adm:ping(Node),

    {Nick, Password} = load_test:register_user(),
    {SessionId, _Pid} = load_test:login(Nick, Password),
    load_test:create_game(SessionId, ?GAME_COUNT),
    {ok, #state{session=SessionId}}.

%%-------------------------------------------------------------------
%% @doc
%% Run
%% * Search for the created game
%% @end
%%-------------------------------------------------------------------
run(test, _KeyGen, _ValueGen, State) ->
    SessionId = State#state.session,
    Games = load_test:search_current(SessionId),
    case length(Games) of
        ?GAME_COUNT ->
            {ok, State};
        _ ->
            {error, search_results_fail, State}
    end.