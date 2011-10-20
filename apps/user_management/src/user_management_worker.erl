-module(user_management_worker).
-behaviour(gen_server).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/bucket.hrl").

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
handle_cast({get_user, Client, Id},
            #state{db_conn = Conn} = State) ->
    Result = get_user(Conn, Id),
    gen_server:reply(Client, Result),
    {noreply, State};
handle_cast({get_user, Client, Type, Key},
            #state{db_conn = Conn} = State) ->
    Result = get_user(Conn, Type, Key),
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
get_user(Conn, Id) ->
    BinId = list_to_binary(integer_to_list(Id)),
    case db_c:get(Conn, ?B_USER, BinId) of
        {ok, RiakObj} ->
            db_obj:get_value(RiakObj);
        {error, Error} ->
            {error, Error};
        Other ->
            erlang:error({error, {unhandled_case, Other, {?MODULE, ?LINE}}})
    end.

get_user(Conn, Type, Key) ->
    {ok, Keys} = db_c:list_keys(Conn, ?B_USER),
    lists:foldl(
      fun(Id, Acc) ->
              {ok, Item} = db_c:get(Conn, ?B_USER, Id),
              Value = db_obj:get_value (Item),
              if
                  Key == '_' ->
                      [db_obj:get_value(Item) | Acc];
                  element (Type, Value) == Key ->
                      [db_obj:get_value(Item) | Acc];
                  true -> Acc
              end
      end,
      [], Keys).

create_user(Conn, undefined, #user{} = User) ->
    Id = db_c:get_unique_id(),
    create_user(Conn, Id, User#user{id = Id});
create_user(Conn, Id, #user{} = User) ->
    BinId = list_to_binary(integer_to_list(Id)),
    DBVal = db_obj:create(?B_USER, BinId, User),
    db_c:put(Conn, DBVal),
    {ok, ReadItem} = db_c:get(Conn, ?B_USER, BinId),
    db_obj:get_value(ReadItem).

is_valid(Conn, Nick, Password) ->
    % This is terrible, but map/reduce erlang code would need to be on the
    % riak node. for now this is the solution...
    % http://lists.basho.com/pipermail/riak-users_lists.basho.com/2010-April/000988.html
    % https://gist.github.com/351659
    % or add a load path to the riak node, where the code is:
    % http://markmail.org/message/xvttgfnufojqbv7w#query:+page:1+mid:xvttgfnufojqbv7w+state:results
    % or use javascript functions ...
    {ok, Keys} = db_c:list_keys(Conn, ?B_USER),
    Result = lists:foldl(
               fun(Key, Acc) ->
                       {ok, Item} = db_c:get(Conn, ?B_USER, Key),
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
