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
%%% @doc Defines how to push messages to different frontends.
%%%
%%%
%%% @since : 17 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(fe_push).

-export([send/4]).

-include_lib("datatypes/include/push_event.hrl").
-include_lib("utils/include/debug.hrl").


%%-------------------------------------------------------------------
%% @doc
%% Sends a push_event to a frontend. for web and mail the general
%% case is sufficient, but for im it needs to be converted to an xml
%% message.
%%
%% @spec send(Type::atom(),
%%            Args::list(),
%%            Pid::pid(),
%%            Event::#push_event{}) ->
%%         ok | {error, {not_a_push_event, any()}}
%% @end
%%-------------------------------------------------------------------
send(im, [Server, ImUser], Pid, Event = #push_event{}) ->
    Msg = xml_message:prepare(Server, ImUser, "chat",
                              fe_messages:get(Event#push_event.type,
                                              Event#push_event.data)),
    ?DEBUG("~p~n", [Msg]),
    Pid ! {route, Server, ImUser, Msg},
    ok;
send(_Type, Args, Pid, Event = #push_event{}) ->
    Pid ! {push, Args, Event},
    ok;
send(_,_,_,Data) ->
    {error, {not_a_push_event, Data}}.
