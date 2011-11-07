%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Tiina Loukusa <loukusa@gmail.com>
%%%
%%% @doc Controller server behaviour, handles calls from the frontend
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------

-module(controller_app_worker).
-behaviour(gen_server).

-include_lib("datatypes/include/user.hrl").

%% ------------------------------------------------------------------
%% Internal API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% eUnit exports - do no use!
%% ------------------------------------------------------------------
-export([resolve_user_conflict/2]).

%% server state
-record(state, {}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Starts a new gen_server and links it to its parent
%% @end
%% [@spec start_link() -> {ok, #state{}}.
%% @end]
%%-------------------------------------------------------------------
-spec start_link() -> {ok, #state{}}.
start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

%%-------------------------------------------------------------------
%% @doc
%% Initiates a ping call to a random controller_app_worker server
%% @end
%% [@spec ping() -> {pong, pid()}.
%% @end]
%%-------------------------------------------------------------------
-spec ping() -> {pong, pid()}.
ping() ->
    gen_server:call(service_worker:select_pid(?MODULE), ping).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Initiates the controller_app_worker
%% @end
%% [@spec init(no_arg::atom()) -> {ok, #state{}}.
%% @end]
%%-------------------------------------------------------------------
-spec init(atom()) -> {ok, #state{}}.
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for creation of a user
%% @end
%% [@spec handle_call({create_user::atom(), Id::Integer(), #user{}},
%%                     From::{pid(), Tag}, #state{}) -> {noreply, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({register, User}, _From, State) ->
    Result = case user_management:create(User) of 
                 {ok, User1 = #user{}} ->
                     SessHist = session_history:create(User1#user.id),
                     session_history:db_put(SessHist),
                     {ok, User1};
                 {error, nick_already_exists} ->
                     {error, nick_already_exists};
                 {error, Error} ->
                     {error, Error};
                 Other ->
                     {error, Other}
             end,
    {reply, Result, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for logging in a user
%%
%% The function checks the credentials of the user by making a call to
%% user_management and if valid, adds the user to the session and returns the
%% session id.
%% On fail, it returns the atom invalid
%% @end
%% [@spec handle_call({login::atom(), #user{}},
%%                     From::{pid(), Tag}, #state{}) ->
%%%         {reply, interger(), #state{}} | {reply, invalid, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({login, Login}, _From, State) ->
    Result =
        case user_management:get_by_idx(#user.nick, Login#user.nick) of
            {ok, {index_list, _IdxList}} ->
                % Multiple nicks in the db, not allowed ...
                {error, nick_not_unique};
            {ok, DbObj} ->
                % user with nick exists
                Id = id_from_user_siblings(DbObj),

                {ok, HObj} = session_history:db_get(Id),
                HistObj = session_history:resolve_history_siblings(HObj),
                Hist = db_obj:get_value(HistObj),
                % @todo kill old session ...

                UserObj = resolve_user_conflict(Hist, DbObj),
                User = db_obj:get_value(UserObj),

                case User#user.password == Login#user.password of
                    false ->
                        {error, invalid_login_data};
                    true ->
                        SessId = session:start(User, Hist),
                        NewHist = session_history:add(Hist, SessId),
                        NewHistObj = db_obj:set_value(HistObj, NewHist),
                        session_history:db_update(NewHistObj),
                        {ok, CheckHistObj} = session_history:db_get(Id),
                        case db_obj:has_siblings(CheckHistObj) of
                            true ->
                                % @todo kill all sibling sessions
                                {error, simultaneous_login};
                            false ->
                                NewUser = User#user{last_session = SessId},
                                NewUserObj = db_obj:set_value(UserObj, NewUser),
                                db:put(NewUserObj),
                                {ok, SessId}
                        end
                end;
            {error, does_not_exist} ->
                {error, invalid_login_data};
            {error, Error} ->
                {error, Error}
        end,
    {reply, Result, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for getting a user
%% @end
%% [@spec handle_call({get_user::atom(), atom(),
%%                     integer()|string()}, From::{pid(), Tag}, #state{}) ->
%%%         {noreply, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({get_user, Id}, From, State) ->
    Result = user_management:get(From, Id),
    {reply, Result, State};
handle_call({get_user, Type, Key}, From, State) ->
    Result = user_management:get(From, Type, Key),
    {reply, Result, State};
handle_call(Request, From, State) ->
    io:format("Received unhandled call: ~p~n", [{Request, From, State}]),
    {noreply, ok, State}.


handle_cast(_Msg, State) ->
    io:format ("received unhandled cast: ~p~n",[{_Msg, State}]),
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
id_from_user_siblings(DbObj) ->
    Obj = case db_obj:has_siblings(DbObj) of
              false ->
                  DbObj;
              true ->
                  % id never changes, so just pick the first obj
                  [H|_] = db_obj:get_siblings(DbObj),
                  H
          end,
    (db_obj:get_value(Obj))#user.id.

%%-------------------------------------------------------------------
%% @doc
%% Resolves siblings with the help of the session history.
%%
%% @spec resolve_user_conflict(#session_history{}, #db_obj{}) -> #db_obj{}
%% @end
%%-------------------------------------------------------------------
resolve_user_conflict(Hist, DbObj) ->
    case db_obj:has_siblings(DbObj) of
        false ->
            DbObj;
        true ->
            Siblings = db_obj:get_siblings(DbObj),
            LastSessions = lists:map(
                             fun(SibObj) ->
                                     (db_obj:get_value(SibObj))#user.last_session
                             end, Siblings),
            Pos = session_history:find_newest(Hist, LastSessions),
            lists:nth(Pos, Siblings)
    end.

