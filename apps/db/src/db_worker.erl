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
-module(db_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib ("utils/include/debug.hrl").

%% server state
-record(state, {db_conn, db_stats_addr}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

ping() ->
    gen_server:call(service_worker:select_pid(?MODULE), ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, RiakIp} = application:get_env(riak_ip),
    {ok, ProtoBufPort} = application:get_env(riak_protobuf_port),
    {ok, DatabasePort} = application:get_env(riak_database_port),
    Riak = {pb, {RiakIp, ProtoBufPort}},
    {ok, Conn} = db_c:connect(Riak),
    {ok, #state{db_conn = Conn, db_stats_addr = {RiakIp, DatabasePort}}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call(ping_riak,
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:ping(Conn),
    {reply, Result, State};

handle_call({get, Bucket, Key},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get(Conn, Bucket, Key),
    {reply, Result, State};
handle_call({get, Bucket, Key, Options},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get(Conn, Bucket, Key, Options),
    {reply, Result, State};

handle_call({get_index, Bucket, IdxTup},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get_index(Conn, Bucket, IdxTup),
    {reply, Result, State};

handle_call({get_values, Bucket, Keys},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get_values(Conn, Bucket, Keys),
    {reply, Result, State};
handle_call({get_values, Bucket, Keys, Timeout},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get_values(Conn, Bucket, Keys, Timeout),
    {reply, Result, State};

handle_call({put, Obj},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:put(Conn, Obj),
    {reply, Result, State};
handle_call({put, Obj, Options},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:put(Conn, Obj, Options),
    {reply, Result, State};

handle_call({delete, Bucket, Key},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:delete(Conn, Bucket, Key),
    {reply, Result, State};
handle_call({delete, Bucket, Key, Options},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:delete(Conn, Bucket, Key, Options),
    {reply, Result, State};

handle_call({empty_bucket, Bucket},
            _From, #state{db_conn=Conn} = State) ->
    Result = case db_c:list_keys(Conn, Bucket) of
                 {ok, Keys} ->
                     lists:map(fun(Key) -> db_c:delete(Conn, Bucket, Key) end,
                               Keys),
                 ok;
                 Error ->
                     Error
             end,
    {reply, Result, State};

handle_call(list_buckets,
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:list_buckets(Conn),
    {reply, Result, State};

handle_call({list_keys, Bucket},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:list_keys(Conn, Bucket),
    {reply, Result, State};

handle_call({get_bucket, Bucket},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:get_bucket(Conn, Bucket),
    {reply, Result, State};

handle_call({set_bucket, Bucket, BucketProps},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:set_bucket(Conn, Bucket, BucketProps),
    {reply, Result, State};

handle_call({mapred, Inputs, Query},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:mapred(Conn, Inputs, Query),
    {reply, Result, State};
handle_call({mapred, Inputs, Query, Timeout},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:mapred(Conn, Inputs, Query, Timeout),
    {reply, Result, State};

handle_call({mapred_bucket, Bucket, Query},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:mapred_bucket(Conn, Bucket, Query),
    {reply, Result, State};
handle_call({mapred_bucket, Bucket, Query, Timeout},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:mapred_bucket(Conn, Bucket, Query, Timeout),
    {reply, Result, State};

handle_call({search, Bucket, Query},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:search(Conn, Bucket, Query),
    {reply, Result, State};

handle_call({search_values, Bucket, Query},
            _From, #state{db_conn=Conn} = State) ->
    Result = db_c:search_values(Conn, Bucket, Query),
    {reply, Result, State};

handle_call(get_db_stats, _From, #state{db_stats_addr = DbStatsAddr} = State) ->
    {IpStr, PortInt} = DbStatsAddr,
    Result = case gen_tcp:connect(IpStr, PortInt, [binary, {packet,0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "GET /stats HTTP/1.0\r\n\r\n"),
            receive_db_stats_data(Socket, []);
        _ ->
            {error, get_stats_start_socket_fail}
    end,
    {reply, Result, State};


handle_call(_Request, _From, State) ->
    ?DEBUG("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast(_Msg, State) ->
    ?DEBUG("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:terminate/2
%% ------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?DEBUG("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

%% ------------------------------------------------------------------
%% gen_server:code_change/3
%% ------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
receive_db_stats_data(Socket, BinAcc) ->
    receive
        {tcp, Socket, Bin} ->
            receive_db_stats_data(Socket, [Bin | BinAcc]);
        {tcp_closed, Socket} ->
            Data = list_to_binary(lists:reverse(BinAcc)),
            % extract Json body, discard header
            case re:run(Data, "\r\n\r\n(.*)",
                        [{capture, all_but_first, list}]) of
                {match, [Json]} ->
                    {ok, Json};
                nomatch ->
                    {error, get_stats_body_fail}
            end;
        {tcp_error, Socket, _Reason} ->
            {error, get_stats_tcp_error}
    after
        5000 ->
            {error, get_stats_timeout}
    end.
