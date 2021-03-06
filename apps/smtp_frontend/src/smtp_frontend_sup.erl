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

-module(smtp_frontend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A, Type), {I, {I, start_link, A}, permanent, 5000, Type, [I]}).
-define(CHILD2(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Domain} = case application:get_env(domain) of
                       undefined ->
                           {ok, Hostname} = inet:gethostname(),
                           {ok,{hostent,FullHostname,[],inet,_,[_]}} =
                               inet:gethostbyname(Hostname),
                           {ok, FullHostname};
                       Else ->
                           Else
                   end,
    {ok, Address} = application:get_env(address),
    {ok, Port} = application:get_env(port),
    {ok, Protocol} = application:get_env(protocol),
    {ok, Family} = application:get_env(family),
    ServerOptions = [[
                      {domain, Domain},
                      {address, Address},
                      {port, Port},
                      {protocol, Protocol},
                      {family, Family},
                      {sessionoptions, [{allow_bare_newlines, fix}]}
                     ]],
    SMTPServer = ?CHILD(gen_smtp_server, [smtp_core, ServerOptions], worker),
    ReplyServer = ?CHILD2(mail_sender, worker),
    {ok, {{one_for_one, 5, 10}, [SMTPServer, ReplyServer]}}.
