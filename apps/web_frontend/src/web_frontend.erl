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
-module(web_frontend).

-export([start/0, run/0]).

start() ->
    {ok, spawn(?MODULE, run, [])}.

%% Start an embedded Yaws server under our own supervisor. This is done so that
%% we can set configuration options directly within Erlang instead of relying on
%% a yaws.conf file.
run() ->
    {ok, Port} = application:get_env(port),
    {ok, ListenList} = application:get_env(listen),
    % Convert address list to tuples so that they can be concatenated with the
    % server config list
    TupleAddr = lists:map(fun inet_parse:address/1, ListenList),
    Listen = [ {listen, Address} || {ok, Address} <- TupleAddr ],
    Id = "embedded",
    Docroot = "./www",
    GconfList = [{id, Id},
                 {logdir, "./log"}],
    SconfList = Listen++[{port, Port},
                 {servername, "treacheroustalks"},
                 {docroot, Docroot},
                 {appmods, [{"/", web_controller, [["js"], ["css"], ["image"], ["page"], ["favicon.ico"]]}]}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(web_frontend_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
