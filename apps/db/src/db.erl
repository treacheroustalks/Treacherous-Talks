%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Andre Hilsendeger <Andre.Hilsendeger@gmail.com>
%%%
%%% @doc DB wrapper, that handles all DB requests.
%%%
%%% Each riak node has a limited amount of supported connections,
%%% Therefore the DB is a service with workers. The db workers perform
%%% all the requests and abstract away the DB connection.
%%% Thus the amount of workers equals the amount of open connections.
%%% @end
%%%
%%% @since : 27 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(db).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([ping_riak/0,
         get/2, get/3,
         get_index/2,
         put/1, put/2,
         delete/2, delete/3,
         list_buckets/0,
         list_keys/1,
         get_bucket/1,
         set_bucket/2,
         mapred/2, mapred/3,
         mapred_bucket/2, mapred_bucket/3,
         get_unique_id/0
        ]).

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------
-define(WORKER, db_worker).
-define(CALL_WORKER(Cmd), gen_server:call(service_worker:select_pid(?WORKER), Cmd)).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc 
%% Ping riak.
%% @end
%%
%% @spec ping_riak() -> pong | pang.
%% @end
%%-------------------------------------------------------------------
ping_riak() ->
    ?CALL_WORKER(ping_riak).


%%-------------------------------------------------------------------
%% @doc
%% Gets a value from the DB.
%% @end
%%
%% [@spec
%%  get(Bucket::binary(), Key::binary()) ->
%%     {ok, db_obj()} | {error, notfound}.
%% @end]
%%-------------------------------------------------------------------
get(Bucket, Key) ->
    ?CALL_WORKER({get, Bucket, Key}).
%%-------------------------------------------------------------------
%% @doc
%% Gets a value from the DB, supplying options.
%% Options:
%%      [{r, 1}] would set r=1 for the request
%%      [{if_modified, VClock}] will return unchanged if the object's vclock matches
%%      [head] only return the object's metadata, the value is set to <<>>
%%      [deletedvclock] return a vclock if a tombstone is encountered
%% @end
%%
%% [@spec
%%  get(Bucket::binary(), Key::binary(), Options::list()) ->
%%     {ok, db_obj()} | {error, notfound}.
%% @end]
%%-------------------------------------------------------------------
get(Bucket, Key, Options) ->
    ?CALL_WORKER({get, Bucket, Key, Options}).


%%-------------------------------------------------------------------
%% @doc
%% Returns a bucket/key pair list for a given index.
%% An index needs to be a binary and end with _bin
%% @end
%%
%% [@spec
%%  get_index(Bucket::binary(), {Index::binary, IndexKey::binary()}) ->
%%     {ok, [ [bucket::binary(), key::binary()] ]} | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
get_index(Bucket, IdxTup={_Index, _IndexKey}) ->
    ?CALL_WORKER({get_index, Bucket, IdxTup}).


%%-------------------------------------------------------------------
%% @doc
%% Stores an object in the database.
%% @end
%%
%% [@spec
%%  put(Obj::db_obj()) ->
%%     ok | {ok, key()} | {error, term()}
%% @end]
%%-------------------------------------------------------------------
put(Obj) ->
    ?CALL_WORKER({put, Obj}).
%%-------------------------------------------------------------------
%% @doc
%% Stores an object in the database, supplying options.
%% Options:
%%      [{w,2}] sets w=2,
%%      [{dw,1}] set dw=1,
%%      [{pw,1}] set pw=1,
%%      [return_body] returns the updated metadata/value
%%      [return_head] returns the updated metadata with the values set as <<>>
%%      [if_not_modified] the put fails unless riakc_obj and database vclocks match
%%      [if_none_match] the put fails if the key already exist
%% @end
%%
%% [@spec
%%  put(Obj::db_obj()) ->
%%     ok | {ok, key()} | {error, term()}
%% @end]
%%-------------------------------------------------------------------
put(Obj, Options) ->
    ?CALL_WORKER({put, Obj, Options}).


%%-------------------------------------------------------------------
%% @doc
%% Deletes a value from the database.
%% @end
%%
%% [@spec
%%  delete(Bucket::binary(), Key::binary()) ->
%%     ok | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
delete(Bucket, Key) ->
    ?CALL_WORKER({delete, Bucket, Key}).
%%-------------------------------------------------------------------
%% @doc
%% Deletes a value from the database, supplying options.
%% Options:
%%      [{rw,2}] sets rw=2 - this is deprecated
%%      [{r,1}] sets r=1
%%      [{w,all}] sets w=all
%%      [{pr,quorum}] sets pr=quorum
%%      [{pw,2}] sets pw=2
%%      [{dw,2}] sets dw=2
%% @end
%%
%% [@spec
%%  delete(Bucket::binary(), Key::binary()) ->
%%     ok | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
delete(Bucket, Key, Options) ->
    ?CALL_WORKER({delete, Bucket, Key, Options}).


%%-------------------------------------------------------------------
%% @doc
%% Lists all the buckets in the riak cluster.
%% @end
%%
%% [@spec
%%  list_buckets() ->
%%     {ok, [binary()]} | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
list_buckets() ->
    ?CALL_WORKER(list_buckets).

%%-------------------------------------------------------------------
%% @doc
%% Lists all keys for a given bucket.
%% @end
%%
%% [@spec
%%  list_keys(Bucket::binary()) ->
%%     {ok, [binary()]}.
%% @end]
%%-------------------------------------------------------------------
list_keys(Bucket) ->
    ?CALL_WORKER({list_keys, Bucket}).

%%-------------------------------------------------------------------
%% @doc
%% Gets the bucket properties.
%% @end
%%
%% [@spec
%%  get_bucket(Bucket::binary()) ->
%%     {ok, bucket_props()} | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
get_bucket(Bucket) ->
    ?CALL_WORKER({get_bucket, Bucket}).
%%-------------------------------------------------------------------
%% @doc
%% Sets the bucket properties.
%% @end
%%
%% [@spec
%%  set_bucket(Bucket::binary(), BucketProps::bucket_props()) ->
%%     ok | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
set_bucket(Bucket, BucketProps) ->
    ?CALL_WORKER({set_bucket, Bucket, BucketProps}).


% the magic map/reduce, no idea how to use those yet ...
%%-------------------------------------------------------------------
%% @doc
%% Perform a map/reduce on the given Inputs.
%% @end
%%
%% [@spec
%%  mapred(Inputs::[{Bucket::binary(), Key::binary()}], 
%%         Query::[MAGIC]) ->
%%     {ok, ...} | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
mapred(Inputs, Query) ->
    ?CALL_WORKER({mapred, Inputs, Query}).
mapred(Inputs, Query, Timeout) ->
    ?CALL_WORKER({mapred, Inputs, Query, Timeout}).


%%-------------------------------------------------------------------
%% @doc
%% Perform a map/reduce on the given bucket.
%% @end
%%
%% [@spec
%%  mapred_bucket(Bucket::binary(), Query::[MAGIC]) ->
%%     {ok, ...} | {error, term()}.
%% @end]
%%-------------------------------------------------------------------
mapred_bucket(Bucket, Query) ->
    ?CALL_WORKER({mapred_bucket, Bucket, Query}).
mapred_bucket(Bucket, Query, Timeout) ->
    ?CALL_WORKER({mapred_bucket, Bucket, Query, Timeout}).


%%-------------------------------------------------------------------
%% @doc
%% Returns a unique id.
%% @end
%%
%% @spec get_unique_id() -> integer().
%% @end
%%-------------------------------------------------------------------
get_unique_id() ->
    db_c:get_unique_id().


%%-------------------------------------------------------------------
%% not sure if we need the streaming for now ...
%%-------------------------------------------------------------------
%stream_list_keys(Bucket) ->
%    gen_server:call(service_worker:select_pid(?WORKER),
%                    {stream_list_keys, Bucket}).
%
%
%mapred_stream(Inputs, Query, ClientPid) ->
%    gen_server:call(service_worker:select_pid(?WORKER),
%                    {mapred_stream, Inputs, Query, ClientPid}).
%mapred_stream(Inputs, Query, ClientPid, Timeout) ->
%    gen_server:call(service_worker:select_pid(?WORKER),
%                    {mapred_stream, Inputs, Query, ClientPid, Timeout}).
%
%
%mapred_bucket_stream(Bucket, Query, ClientPid, Timeout) ->
%    gen_server:call(service_worker:select_pid(?WORKER),
%                    {mapred_bucket_stream, Bucket, Query, ClientPid, Timeout}).




%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------
