%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Wrapper for Riak clients.  Supports HTTP and Protocol Buffers.
-module(db_c).

-include_lib ("eunit/include/eunit.hrl").

-export([connect/0, connect/1, connect/2,
         disconnect/1,
         ping/1,
         get_client_id/1, set_client_id/2,
         get_server_info/1,
         get/3, get/4,
         put/2, put/3,
         delete/3, delete/4,
         list_buckets/1,
         list_keys/2,
         stream_list_keys/2,
         get_bucket/2,
         set_bucket/3,
         mapred/3, mapred/4,
         mapred_stream/4, mapred_stream/5,
         mapred_bucket/3, mapred_bucket/4,
         mapred_bucket_stream/5]).

-record(db_c, {module, client}).

-define(DEFAULT_RIAK, {pb, {"127.0.0.1", 8087}}).

-define(PASS0(RC, Command),
        (RC#db_c.module):Command(RC#db_c.client)).
-define(PASS1(RC, Command, P1),
        (RC#db_c.module):Command(RC#db_c.client,
                                      P1)).
-define(PASS2(RC, Command, P1, P2),
        (RC#db_c.module):Command(RC#db_c.client,
                                      P1, P2)).
-define(PASS3(RC, Command, P1, P2, P3),
        (RC#db_c.module):Command(RC#db_c.client,
                                      P1, P2, P3)).
-define(PASS4(RC, Command, P1, P2, P3, P4),
        (RC#db_c.module):Command(RC#db_c.client,
                                      P1, P2, P3, P4)).

connect() ->
    connect (application:get_env (riak)).

connect({Channel, {Host, Port}}) when is_list (Host), 
                                      is_integer (Port) ->
    connect(random_client_id(), {Channel, {Host, Port}}).
connect(ClientId, {Channel, Node}) when is_binary(ClientId) ->
    case {Channel, Node} of
        {http, Node} ->
            connect_http(ClientId, Node);
        {pb, Node} ->
            connect_pb(ClientId, Node)
    end.

connect_http(ClientId, {IP, Port, Prefix}) ->
    C = rhc:create(IP, Port, Prefix, [{client_id, ClientId}]),
    {ok, #db_c{module = rhc,
              client = C}}.

connect_pb(ClientId, {IP, Port}) ->
    {ok, C} = riakc_pb_socket:start_link(IP, Port),
    ok = riakc_pb_socket:set_client_id(C, ClientId),
    {ok, #db_c{module = riakc_pb_socket,
              client = C}}.

disconnect(#db_c{module=riakc_pb_socket, client=C}) ->
    riakc_pb_socket:stop(C);
disconnect(_DB_C) ->
    ok.

ping(RC) ->
    ?PASS0(RC, ping).

get_client_id(RC) ->
    ?PASS0(RC, get_client_id).

set_client_id(#db_c{module=rhc, client=C}, ClientId) ->
    connect_http(ClientId,
                 {rhc:ip(C), rhc:port(C), rhc:prefix(C)});
set_client_id(DB_C=#db_c{module=riakc_pb_socket, client=C},
              ClientId) ->
    riakc_pb_socket:set_client_id(C, ClientId),
    {ok, DB_C}.

get_server_info(RC) -> ?PASS0(RC, get_server_info).

get(RC, Bucket, Key) ->
    to_db_obj(?PASS2(RC, get, Bucket, Key)).
get(RC, Bucket, Key, Options) ->
    to_db_obj(?PASS3(RC, get, Bucket, Key, Options)).

to_db_obj({ok, RCObj}) ->
    {ok, db_obj:from_riakc_obj(RCObj)};
to_db_obj(Error) ->
    Error.

put(RC, Obj) ->
    ?debugVal (RC),
    ?debugVal (Obj),
    ?PASS1(RC, put, db_obj:to_riakc_obj(Obj)).
put(RC, Obj, Options) ->
    ?PASS2(RC, put, db_obj:to_riakc_obj(Obj), Options).

delete(RC, Bucket, Key) ->
    ?PASS2(RC, delete, Bucket, Key).
delete(RC, Bucket, Key, Options) ->
    ?PASS3(RC, delete, Bucket, Key, Options).

list_buckets(RC) ->
    ?PASS0(RC, list_buckets).

list_keys(RC, Bucket) ->
    ?PASS1(RC, list_keys, Bucket).

stream_list_keys(RC, Bucket) ->
    ?PASS1(RC, stream_list_keys, Bucket).

get_bucket(RC, Bucket) ->
    ?PASS1(RC, get_bucket, Bucket).

set_bucket(RC, Bucket, BucketProps) ->
    ?PASS2(RC, set_bucket, Bucket, BucketProps).

mapred(RC, Inputs, Query) ->
    ?PASS2(RC, mapred, Inputs, Query).
mapred(RC, Inputs, Query, Timeout) ->
    ?PASS3(RC, mapred, Inputs, Query, Timeout).

mapred_stream(RC, Inputs, Query, ClientPid) ->
    ?PASS3(RC, mapred_stream, Inputs, Query, ClientPid).
mapred_stream(RC, Inputs, Query, ClientPid, Timeout) ->
    ?PASS4(RC, mapred_stream, Inputs, Query, ClientPid, Timeout).

mapred_bucket(RC, Bucket, Query) ->
    ?PASS2(RC, mapred_bucket, Bucket, Query).
mapred_bucket(RC, Bucket, Query, Timeout) ->
    ?PASS3(RC, mapred_bucket, Bucket, Query, Timeout).

mapred_bucket_stream(RC, Bucket, Query, ClientPid, Timeout) ->
    ?PASS4(RC, mapred_bucket_stream, Bucket, Query, ClientPid, Timeout).

%% INTERNAL

random_client_id() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    Id = erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]),
    base64:encode(<<Id:32>>).
