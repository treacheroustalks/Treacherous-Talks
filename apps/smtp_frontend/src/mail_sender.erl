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
%%% @author  Lin and Rahim  <2287727@live.cn && r.k.mohammadi@gmail.com>
%%% @doc
%%%   this module is used by controller to send message to user.
%%%   whenever you want to message to user send a message to this module
%%%   which is register by its name.
%%%
%%%  @since Nov 16, 2011 By Bermuda triangle
%%%  @end
%%% -------------------------------------------------------------------
-module(mail_sender).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("datatypes/include/push_event.hrl").
-include_lib("utils/include/debug.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, get_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).
%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    io:format(user, "mail_sender####~p~p~n", ["start link", self()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_arg, []).

-spec get_pid() -> pid().
get_pid() ->
    gen_server:call(?MODULE, get_pid).

%% ====================================================================
%% Server functions
%% ====================================================================

init(no_arg) ->
    io:format(user, "mail_sender####~p~p~n", ["init", self()]),
    {ok, #state{}}.

handle_call(get_pid, _From, State) ->
    Reply = self(),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc
%%   receive message from controller and forward it to the user client
%% @end
%%------------------------------------------------------------------------------
handle_info({push, [ServerAddr, UserAddr],
     _Event = #push_event{type = Type, data =Data}}, State)->
    [_, ToHost] = string:tokens(UserAddr, "@"),

    ?DEBUG("mail_sender####~p ~p~n", ["handle_info", _Event]),
    smtp_output:send_mail(ServerAddr, UserAddr, ToHost,
                           fe_message:get(Type, Data)),
    {noreply, State};
handle_info(Info, State) ->
    io:format(user, "[mail_sender] unhandeled message ~n~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
