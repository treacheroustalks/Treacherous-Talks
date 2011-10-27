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

%% server state
-record(state, {db_conn}).
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
    {ok, Riak} = application:get_env(riak),
    {ok, Conn} = db_c:connect(Riak),
    {ok, #state{db_conn = Conn}}.

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

handle_call(_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
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
    io:format ("[~p] terminated ~p: reason: ~p, state: ~p ~n",
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

