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
-module(db_app).
-vsn("1.0.0").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib ("utils/include/debug.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?DEBUG("[~p] starting~n", [?MODULE]),
    {ok, RiakIp} = application:get_env(riak_ip),
    {ok, ProtoBufPort} = application:get_env(riak_protobuf_port),
    Riak = {pb, {RiakIp, ProtoBufPort}},
    {ok, Conn} = db_c:connect(Riak),
    case ping_riak(Conn, 20) of
        pong ->
            db_c:disconnect(Conn),
            db_sup:start_link();
        pang ->
            db_c:disconnect(Conn),
            {error, riak_not_running}
    end.

stop(_State) ->
    ok.


ping_riak(_Conn, 0) ->
    pang;
ping_riak(Conn, Tries) ->
    case db_c:ping(Conn) of
        pong ->
            pong;
        _ ->
            timer:sleep(1000),
            ping_riak(Conn, Tries-1)
    end.
