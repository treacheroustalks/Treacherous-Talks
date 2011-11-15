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
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc Provides functionality that is used by the workers of a
%%% service applications.
%%% @end
%%%
%%% @since : 14 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(service_worker).

%% Public interface
-export([join_group/1, get_members/1, select_pid/1]).

%%-------------------------------------------------------------------
%% @doc
%% Adds the calling process to the group Name.
%% @end
%% @spec join_group(Name::atom()) -> ok
%% @end
%%-------------------------------------------------------------------
-spec join_group(atom()) -> ok.
join_group(Name) ->
    pg2:create(Name),
    pg2:join(Name, self()).

%%-------------------------------------------------------------------
%% @doc
%% Returns a list of the pids that are in the group Name.
%% @end
%% @spec get_members(Name::atom()) ->
%%       [pid()] | {error, {no_such_group, Name}}
%% @end
%%-------------------------------------------------------------------
-spec get_members(atom()) -> [pid()] | {error, {no_such_group, atom()}}.
get_members(Name) ->
    pg2:get_members(Name).

%%-------------------------------------------------------------------
%% @doc
%% Selects a process from a given group.
%% @end
%% @spec select_pid(Name::atom()) -> pid() | {error, Reason}
%% @end
%%-------------------------------------------------------------------
-spec select_pid(atom()) -> pid() | {error, term()}.
select_pid(Name) ->
    pg2:get_closest_pid(Name).
