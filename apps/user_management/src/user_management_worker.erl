-module(user_management_worker).
-behaviour(gen_server).

-include_lib("datatypes/include/user.hrl").

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

-define(USER_BUCKET, "user").
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

handle_call({create_user, Id, #user{} = User},
            _From, #state{db_conn = Conn} = State) ->
    Result = create_user(Conn, Id, User),
    {reply, Result, State};
handle_call({is_valid, Nick, Password},
            _From, #state{db_conn = Conn} = State) ->
    Result = is_valid(Conn, Nick, Password),
    {reply, Result, State};
handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call(_Request, _From, State) ->
    io:format ("[~p] received unhandled call: ~p~n",[?MODULE, {_Request, _From, State}]),
    {noreply, ok, State}.

handle_cast({create_user, Client, Id, #user{} = User},
            #state{db_conn = Conn} = State) ->
    Result = create_user(Conn, Id, User),
    gen_server:reply(Client, Result),
    {noreply, State};
handle_cast({is_valid, Client, Nick, Password},
            #state{db_conn = Conn} = State) ->
    Result = is_valid(Conn, Nick, Password),
    gen_server:reply(Client, Result),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format ("[~p] received unhandled cast: ~p~n",[?MODULE, {_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format ("[~p] terminated ~p: reason: ~p, state: ~p ~n",
               [?MODULE, self(), _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
create_user(Conn, undefined, #user{} = User) ->
    Id = db_c:get_unique_id(),
    create_user(Conn, Id, User#user{id = Id});
create_user(Conn, Id, #user{} = User) ->
    DBVal = db_obj:create(<<?USER_BUCKET>>, <<Id>>, User),
    db_c:put(Conn, DBVal),
    {ok, ReadItem} = db_c:get(Conn, <<?USER_BUCKET>>, <<Id>>),
    db_obj:get_value(ReadItem).

is_valid(Conn, Nick, Password) ->
    % This is terrible, but map/reduce erlang code would need to be on the
    % riak node. for now this is the solution...
    % http://lists.basho.com/pipermail/riak-users_lists.basho.com/2010-April/000988.html
    % https://gist.github.com/351659
    % or add a load path to the riak node, where the code is:
    % http://markmail.org/message/xvttgfnufojqbv7w#query:+page:1+mid:xvttgfnufojqbv7w+state:results
    % or use javascript functions ...
    {ok, Keys} = db_c:list_keys(Conn, <<?USER_BUCKET>>),
    Result = lists:foldl(
               fun(Key, Acc) ->
                       {ok, Item} = db_c:get(Conn, <<?USER_BUCKET>>, Key),
                       Val = db_obj:get_value(Item),
                       case Val of
                           #user{nick = Nick, password = Password} ->
                               [Val | Acc];
                           _ ->
                               Acc
                       end
               end, [], Keys),
    case Result of
        [] ->
            false;
        [User | _] ->
            User
    end.
