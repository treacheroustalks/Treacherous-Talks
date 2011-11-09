%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc This module provides functions for session ids.
%%%
%%% @end
%%%
%%% @since :  2 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(session_id).

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------
-export([
         to_pid/1,
         from_pid/1
        ]).

%% ------------------------------------------------------------------
%% Interface Function Implementation
%% ------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Converts a session Id to a Pid.
%%
%% @spec to_pid(Id::string()) -> pid() | {error, invalid_id}
%% @end
%%-------------------------------------------------------------------
to_pid(Id) when is_list(Id) ->
    try binary_to_term(base64:decode(Id)) of
        Val -> Val
    catch
        _:_ -> {error, invalid_id}
    end;
to_pid(_) ->
    {error, invalid_id}.

%%-------------------------------------------------------------------
%% @doc
%% Converts a Pid to a session Id.
%%
%% @spec from_pid(Pid::pid()) -> string() | {error, invalid_pid}
%% @end
%%-------------------------------------------------------------------
from_pid(Pid) when is_pid(Pid) ->
    base64:encode_to_string(term_to_binary(Pid));
from_pid(_) ->
    {error, invalid_pid}.
