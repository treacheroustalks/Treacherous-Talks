-module (db).

-define (SERVER, db_server).

-include_lib ("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Public Interface
%% ------------------------------------------------------------------

-export ([ping/0,
          start_link/0,
          get_state/0,
          add_user/1,
          get_user/1,
          delete_user/1,
          add_link/3,
          follow_link/3,
          john/0,
          martha/0,
          stop/0]).

-include_lib ("include/user.hrl").

%% @doc returns the atom pong
-spec ping () -> pong.
ping () ->
    gen_server:call(?SERVER,ping).

%% @doc use this function to start the server
start_link () ->
    ?SERVER:start_link ().

%% @doc stops the server. Riak continues to run.
-spec stop () -> ok.
stop () ->
    gen_server:call (?SERVER, stop),
    ok.

%% @doc adds a user as defined in include/user.hrl
-spec add_user (#user{}) -> {ok, binary ()}.
add_user (User) when is_record (User, user) ->
    add (?SERVER:db_bucket (user), User).

%% @doc returns the user as a tuple-list, [unknown], if not found
-spec get_user (binary ()) -> [#user{}].
get_user (ID) when is_binary (ID) ->
    Tuple=get_tuple (?SERVER:db_bucket (user), ID),
    Tuple.

%% @doc deletes the user;
%% returns ok, even if the user does not exist
-spec delete_user (binary ()) -> ok.
delete_user (ID) ->
    gen_server:call (?SERVER, {delete_user, ID}).

%% @doc returns the state
%% use only for debugging! state is subject to heavy change and will remain so
get_state () ->
    gen_server:call (?SERVER,get_state).

%% @doc returns a user for testing purposes
%% @reference martha
-spec john () -> #user{}.
john () ->
    #user{
       email="john@host.com",
       password="asdf1234",
       name="John User",
       channel=web,
       last_ip={123,123,123,123},
       last_login={{2011,10,01},{14,20,27}},
       score=123,
       date_created={{2011,10,01},{14,20,27}},
       date_updated={{2011,10,01},{14,20,27}}
      }.

%% @doc returns a user for testing purposes
%% @reference john
-spec martha () -> #user{}.
martha () ->
    #user{
         email="martha@host.pcs",
         password="martha123password",
         name="Martha User",
         role=moderator,
         channel=xmpp,
         last_ip={321,321,321,321},
         last_login={{2011,10,05},{09,30,12}},
         score=213,
         date_created={{2011,08,01},{14,20,27}},
         date_updated={{2011,08,05},{18,07,43}}
        }.

add_link (LinkName, {FromBucket, FromID}, {ToBucket, ToID}) ->
    [Item] = get (FromBucket, FromID),
    gen_server:call (?SERVER, {add_link,
                               Item,
                               LinkName,
                               {FromBucket, FromID},
                               {ToBucket, ToID}}).

%% ------------------------------------------------------------------
%% Private
%% ------------------------------------------------------------------

add (Bucket, Item) when is_tuple (Item) ->
    add (Bucket,list_to_binary (io_lib:format ("~p.",[Item])));
add (Bucket, Item) when is_binary (Item) ->
    gen_server:call (?SERVER,{add, Bucket, Item}).

-spec get (binary (), binary ()) -> tuple ().
get (Bucket, ID) when is_binary (Bucket), is_binary (ID) ->
    gen_server:call (?SERVER, {get, Bucket, ID}).

say (Something) ->
    io:format ("~p: ~p~n",[?MODULE, Something]).

-spec get_tuple (binary (), binary ()) -> tuple ().
get_tuple (Bucket, ID) ->
    Contents = get (Bucket,ID),
    Ret=lists:map (fun (C) ->
                           ?debugVal (C),
                           {ok, Tokens, _} = erl_scan:string (
                                               binary_to_list (C)),
                           ?debugVal (Tokens),
                           {ok, Tuple} = erl_parse:parse_term (Tokens),
                           say (Tuple),
                           Tuple
               end,
               Contents),
    ?debugVal (Ret),
    Ret.

-spec follow_link (binary (), binary (), binary ()) -> ok | [tuple ()] | [unknown].
follow_link (LinkName, Bucket, ID) ->
    RObj = gen_server:call (?SERVER, {get, Bucket, ID}),
    gen_server:call (?SERVER, {follow_link, RObj, LinkName}),
    [unknown].
