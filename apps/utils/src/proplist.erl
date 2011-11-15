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
%%%-------------------------------------------------------------------
%%% @doc proplist
%%%
%%% A module for our property list utilities.
%%%
%%% @end
%%--------------------------------------------------------------------

-module(proplist).

-export([update_record/2]).

%%------------------------------------------------------------------------------
%% @doc update record via a proplist
%%  Input: Arg1: OldUser#user
%%         Arg2: [{#user.name, "username"}, {#user.password, "xxxx"}]
%%
%%  Output: #user{name="username", password="xxxx"}
%% @end
%%------------------------------------------------------------------------------
update_record(Old, [{_, field_missing}|Rest]) ->
    update_record(Old, Rest);
update_record(Old, [{Field, Value}|Rest]) ->
    update_record(setelement(Field, Old, Value), Rest);
update_record(Updated, []) ->
    Updated.
