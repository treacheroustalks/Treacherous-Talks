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
