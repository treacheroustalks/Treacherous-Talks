-module(db_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib ("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, db_bucket/1]).

-include_lib ("include/user.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {"127.0.0.1",8081}, []),
    io:format ("~p~n",[{local, ?SERVER}]),
    ok.

say (Something) ->
    io:format ("~p~n",[Something]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Host,Port}) ->
    io:format ("db_server starting~n"),
    say ("connecting to local node.."),
    case riakc_pb_socket:start_link (Host, Port) of
    {ok, Pid} ->
        say ("connected!"),
        Db_Node = {db_node, Host, 8081, Pid},
        {ok, [Db_Node]};
    Error ->
        say ("not connected!"),
        say (Error),
        erlang:error ({error, no_connection, Error})
    end.

%% TODO this could become an env
db_bucket (user) ->
    <<"user">>.

handle_call (stop, _From, State) ->
    {stop, normal, State};
handle_call ({add_link,
          _Item,
          LinkName,
          {FromBucket, FromID},
          {ToBucket, ToID}}, _From, State) ->
    say (handle_call_add_link),
    say (LinkName),
    ?debugVal ({FromBucket, FromID}),
    ?debugVal ({ToBucket, ToID}),
    {db_node, _Ip, _Port, Pid}=lists:keyfind (db_node,1, State),
    {ok,Item} = riakc_pb_socket:get (Pid, FromBucket, FromID),
    ?debugVal (Item),
    Metadata=dict:store (<<"Links">>,
                         [{{ToBucket, ToID}, LinkName}],
                         dict:new ()),
    ?debugVal (Metadata),
    Item1=riakc_obj:update_metadata (Item,Metadata),
    ?debugVal (Item1),
    ok = riakc_pb_socket:put (Pid, Item1),
    {reply, Item1, State};
handle_call ({follow_link, _RObj, _LinkName}, _From, State) ->
    {reply, [<<"unknown.">>], State};
handle_call (get_state, _From, State) ->
    {reply, State, State};
handle_call ({add, Bucket, BinaryItem}, _From, State) ->
    RiakObject = riakc_obj:new (Bucket, undefined, BinaryItem, "text/html"),
    {db_node, _Ip, _Port, Pid}=lists:keyfind (db_node,1, State),
    case riakc_pb_socket:put (Pid, RiakObject) of
        {ok, Key} ->
            {reply, {ok, Key}, State};
        Other ->
            erlang:error ({error, no_write, {original_reply, Other}})
    end;
handle_call ({get, Bucket, ID}, _From, State) ->
    {db_node, _Ip, _Port, Pid}=lists:keyfind (db_node,1, State),
    ?debugVal ({get,Bucket,ID}),
    case riakc_pb_socket:get (Pid, Bucket, ID) of
        {ok, Reply} ->
            Values = riakc_obj:get_values (Reply),
            ?debugVal (Values),
            {reply, Values, State};
        {error,_Reason} ->
            {reply, [<<"unknown.">>], State}
    end;
handle_call ({delete_user, ID}, _From, State) ->
    {db_node, _Ip, _Port, Pid}=lists:keyfind (db_node,1, State),
    Reply = riakc_pb_socket:delete (Pid, db_bucket (user), ID),
    {reply, Reply, State};
handle_call (ping, _From, State) ->
    {reply, pong, State};
handle_call (_Request, _From, State) ->
    io:format ("received unhandled call: ~p~n",[{_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format ("~p terminated: reason: ~p, state: ~p ~n",[?SERVER, _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

