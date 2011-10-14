%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
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
