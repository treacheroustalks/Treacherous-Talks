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
-compile({no_auto_import,[put/2]}).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([
         delete/2, delete/3,
         empty_bucket/1,
         empty_db/0,
         get/2, get/3,
         get_bucket/1,
         get_db_stats/0,
         get_key_filter/2,
         get_resolve/4, get_resolve/5,
         get_unique_id/0,
         get_values/2, get_values/3,
         int_to_bin/1, int_to_bin/2,
         list_buckets/0,
         list_keys/1,
         mapred/2, mapred/3,
         mapred_bucket/2, mapred_bucket/3,
         ping_riak/0,
         put/1, put/2,
         search/2, search_values/2,
         set_bucket/2
        ]).

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------
-define(WORKER, db_worker).
-define(CALL_WORKER(Cmd), try gen_server:call(
                                service_worker:select_pid(?WORKER), Cmd)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Ping riak.
%%
%% @spec ping_riak() -> pong | pang
%% @end
%%-------------------------------------------------------------------
ping_riak() ->
    ?CALL_WORKER(ping_riak).


%%-------------------------------------------------------------------
%% @doc
%% Gets a value from the DB.
%%
%% @spec
%%  get(Bucket::binary(), Key::binary()) ->
%%     {ok, db_obj()} | {error, notfound}
%% @end
%%-------------------------------------------------------------------
get(Bucket, Key) ->
    ?CALL_WORKER({get, Bucket, Key}).
%%-------------------------------------------------------------------
%% @doc
%% Gets a value from the DB, supplying options.
%% Options:
%%      [{r, 1}] would set r=1 for the request
%%      [{if_modified, VClock}] will return unchanged if the object's vclock matches
%%      [head] only return the object's metadata, the value is set to binary
%%      [deletedvclock] return a vclock if a tombstone is encountered
%%
%% @spec
%%  get(Bucket::binary(), Key::binary(), Options::list()) ->
%%     {ok, db_obj()} | {error, notfound}
%% @end
%%-------------------------------------------------------------------
get(Bucket, Key, Options) ->
    ?CALL_WORKER({get, Bucket, Key, Options}).


%%-------------------------------------------------------------------
%% @doc
%% Same as get/2, but tries to resolve siblings with the session
%% history, if any. Where Field is the position where the session is
%% stored in the record.
%%
%% @spec
%%  get_resolve(Bucket::binary(), Key::binary(), #session_history{}, integer()) ->
%%     {ok, db_obj()} | {error, notfound}
%% @end
%%-------------------------------------------------------------------
get_resolve(Bucket, Key, Hist, Field) ->
    case get(Bucket, Key) of
        {ok, DbObj} ->
            {ok, session_history:resolve_conflict(Hist, DbObj, Field)};
        Other ->
            Other
    end.

%%-------------------------------------------------------------------
%% @doc
%% Same as get/3, but tries to resolve siblings with the session
%% history, if any.
%%
%% @end
%%-------------------------------------------------------------------
get_resolve(Bucket, Key, Options, Hist, Field) ->
    case get(Bucket, Key, Options) of
        {ok, DbObj} ->
            {ok, session_history:resolve_conflict(Hist, DbObj, Field)};
        Other ->
            Other
    end.

%%-------------------------------------------------------------------
%% @doc
%% Gets a list of values from the DB.
%%
%% @spec
%%  get_values(Bucket::binary(), Keys::[binary()]) ->
%%     {ok, [any()]} | {error, any()}
%% @end
%%-------------------------------------------------------------------
get_values(Bucket, Keys) ->
    ?CALL_WORKER({get_values, Bucket, Keys}).

%%-------------------------------------------------------------------
%% @doc
%% Gets a list of values from the DB.
%%
%% @spec
%%  get_values(Bucket::binary(), Keys::[binary()], Timeout::integer()) ->
%%     {ok, [any()]} | {error, any()}
%% @end
%%-------------------------------------------------------------------
get_values(Bucket, Keys, Timeout) ->
    ?CALL_WORKER({get_values, Bucket, Keys, Timeout}).

%%-------------------------------------------------------------------
%% @doc
%% Returns a list of values according to a key filter, example:
%% get_key_filter( `<<"message_unread">>',
%%                [[`<< "ends_with" >>', db:int_to_bin(UserId)]]).
%% This would return all object values in the bucket "message_unread",
%% where the key ends with the UserId.
%% More information: http://wiki.basho.com/Key-Filters.html
%%
%% @spec
%%  get_key_filter(Bucket::binary(), Query) ->
%%     {ok, [list()]} | {error, term()}
%% @end
%%-------------------------------------------------------------------
get_key_filter(Bucket, KeyFilter) ->
    ?CALL_WORKER({get_key_filter, Bucket, KeyFilter}).


%%-------------------------------------------------------------------
%% @doc
%% Stores an object in the database.
%%
%% @spec
%%  put(Obj::db_obj()) ->
%%     ok | {ok, key()} | {error, term()}
%% @end
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
%%      [return_head] returns the updated metadata with the values set as binary
%%      [if_not_modified] the put fails unless riakc_obj and database vclocks match
%%      [if_none_match] the put fails if the key already exist
%% There is a special case for the options: we extended the options to
%% also understand `{w,0}', this works only if `{w,0}' is the FIRST item
%% in the option-list (for performance reasons). `{w,0}' means that a process
%% will be spawned that does the write, `put/2' will return immediately.
%% @spec
%%  put(Obj::db_obj(), Options::list()) ->
%%     ok | {ok, key()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
put(Obj, [{w,0} | Rest]) ->
    spawn (fun() -> put(Obj, Rest) end),
    ok;
put(Obj, Options) ->
    ?CALL_WORKER({put, Obj, Options}).

%%-------------------------------------------------------------------
%% @doc
%% Deletes a value from the database.
%%
%% @spec
%%  delete(Bucket::binary(), Key::binary()) ->
%%     ok | {error, term()}
%% @end
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
%%
%% @spec
%%  delete(Bucket::binary(), Key::binary(), Options::list()) ->
%%     ok | {error, term()}
%% @end
%%-------------------------------------------------------------------
delete(Bucket, Key, Options) ->
    ?CALL_WORKER({delete, Bucket, Key, Options}).

%%-------------------------------------------------------------------
%% @doc
%%  Deletes all documents in given bucket.
%%
%% @spec
%%  empty_bucket(Bucket::binary()) ->
%%     ok | {error, Error}
%% @end
%%-------------------------------------------------------------------
empty_bucket(Bucket) ->
    ?CALL_WORKER({empty_bucket, Bucket}).

%%-------------------------------------------------------------------
%% @doc
%%  Deletes all documents in all buckets.
%% @end
%%-------------------------------------------------------------------
-spec empty_db() -> ok.
empty_db() ->
    {ok, Buckets} = list_buckets(),
    lists:foreach (fun empty_bucket/1, Buckets).

%%-------------------------------------------------------------------
%% @doc
%% Lists all the buckets in the riak cluster.
%%
%% @spec
%%  list_buckets() ->
%%     {ok, [binary()]} | {error, term()}
%% @end
%%-------------------------------------------------------------------
list_buckets() ->
    ?CALL_WORKER(list_buckets).

%%-------------------------------------------------------------------
%% @doc
%% Lists all keys for a given bucket.
%% WARNING: To be used only for debugging and not for regular use
%%
%% @spec
%%  list_keys(Bucket::binary()) ->
%%     {ok, [binary()]}
%% @end
%%-------------------------------------------------------------------
list_keys(Bucket) ->
    ?CALL_WORKER({list_keys, Bucket}).

%%-------------------------------------------------------------------
%% @doc
%% Gets the bucket properties.
%%
%% @spec
%%  get_bucket(Bucket::binary()) ->
%%     {ok, bucket_props()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
get_bucket(Bucket) ->
    ?CALL_WORKER({get_bucket, Bucket}).

%%-------------------------------------------------------------------
%% @doc
%% Sets the bucket properties.
%%
%% @spec
%%  set_bucket(Bucket::binary(), BucketProps::bucket_props()) ->
%%     ok | {error, term()}
%% @end
%%-------------------------------------------------------------------
set_bucket(Bucket, BucketProps) ->
    ?CALL_WORKER({set_bucket, Bucket, BucketProps}).


%%-------------------------------------------------------------------
%% @doc
%% Perform a map/reduce on the given Inputs.
%%
%% @spec
%%  mapred(Inputs::[{Bucket::binary(), Key::binary()}],
%%         Query::[MAGIC]) ->
%%     {ok, list()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
mapred(Inputs, Query) ->
    ?CALL_WORKER({mapred, Inputs, Query}).
mapred(Inputs, Query, Timeout) ->
    ?CALL_WORKER({mapred, Inputs, Query, Timeout}).


%%-------------------------------------------------------------------
%% @doc
%% Perform a map/reduce on the given bucket.
%%
%% @spec
%%  mapred_bucket(Bucket::binary(), Query::[MAGIC]) ->
%%     {ok, list()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
mapred_bucket(Bucket, Query) ->
    ?CALL_WORKER({mapred_bucket, Bucket, Query}).
mapred_bucket(Bucket, Query, Timeout) ->
    ?CALL_WORKER({mapred_bucket, Bucket, Query, Timeout}).


%%-------------------------------------------------------------------
%% @doc
%% Perform a search on the given bucket.
%%
%% @spec
%%  search(Bucket::binary(), Query::[MAGIC]) ->
%%     {ok, list()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
search(Bucket, Query) ->
    ?CALL_WORKER({search, Bucket, Query}).

%%-------------------------------------------------------------------
%% @doc
%% Perform a search on the given bucket and returns the values of all
%% results
%%
%% @spec
%%  search_values(Bucket::binary(), Query::[MAGIC]) ->
%%     {ok, list()} | {error, term()}
%% @end
%%-------------------------------------------------------------------
search_values(Bucket, Query) ->
    ?CALL_WORKER({search_values, Bucket, Query}).

%%-------------------------------------------------------------------
%% @doc
%% Allow operator to view database system status
%%
%% @end
%%-------------------------------------------------------------------
get_db_stats() ->
    ?CALL_WORKER(get_db_stats).

%%-------------------------------------------------------------------
%% @doc
%% Returns a unique id.
%%
%% @spec get_unique_id() -> integer()
%% @end
%%-------------------------------------------------------------------
get_unique_id() ->
    db_c:get_unique_id().

%%-------------------------------------------------------------------
%% @doc
%% Converts an integer id to the expected binary format.
%%
%% @spec int_to_bin(Id::integer()) -> binary()
%% @end
%%-------------------------------------------------------------------
int_to_bin(Id) ->
    int_to_bin(Id, "").

%%-------------------------------------------------------------------
%% @doc
%% Converts an integer id to the expected binary format with a given
%% suffix.
%%
%% @spec int_to_bin(Id::integer(), Suffix::string()) -> binary()
%% @end
%%-------------------------------------------------------------------
int_to_bin(Id, Suffix) ->
    list_to_binary(integer_to_list(Id) ++ Suffix).
