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
                 {error, Error} ->
                     {error, Error};
                 User1 = #user{} ->
                     {ok, User1}
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
handle_call({login, User}, _From, State) ->
    Result = case user_management:is_valid(User#user.nick, User#user.password) of
                 false ->
                     {error, invalid_login_data};
                 User1 = #user{} ->
                     {ok, session:start(User1)}
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

