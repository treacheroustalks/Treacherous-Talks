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

-include_lib("utils/include/debug.hrl").

%% server state
-record(state, {}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Starts a new gen_server and links it to its parent
%% @end
%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid ()} | ignore | {error, _}.
start_link() ->
    gen_server:start_link(?MODULE, no_arg, []).

%%-------------------------------------------------------------------
%% @doc
%% Initiates a ping call to a random controller_app_worker server
%% @end
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
%%-------------------------------------------------------------------
-spec init(atom()) -> {ok, #state{}}.
init(no_arg) ->
    service_worker:join_group(?MODULE),
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, {pong, self()}, State};
handle_call({system_stats, OutputType}, _From, State) ->
    case OutputType of
        string ->
            Stats = system_stats:get_system_stats(string);
        io_format ->
            Stats = system_stats:get_system_stats(io_format)
    end,
    {reply, Stats, State};
%%-------------------------------------------------------------------
%% @doc
%% Handles call for creation of a user
%% @end
%% [@spec handle_call({register::atom(), #user{}},
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
                     {error, Error}
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
%% [@spec handle_call({login::atom(), {#user{}, #push_receiver{}}},
%%                     From::{pid(), Tag}, #state{}) ->
%%%         {reply, interger(), #state{}} | {reply, invalid, #state{}}.]
%% @end
%%-------------------------------------------------------------------
handle_call({login, {Login, PushInfo}}, _From, State) ->
    Result =
        case user_management:get_id(#user.nick, Login#user.nick) of
            {ok, {index_list, _IdxList}} ->
                % Multiple nicks in the db, not allowed ...
                {error, nick_not_unique};
            {ok, UserId} ->
                % user with nick exists
                {ok, DbObj} = user_management:get_db_obj(UserId),
                Id = id_from_user_siblings(DbObj),
                {ok, HObj} = session_history:db_get(Id),
                HistObj = session_history:resolve_history_siblings(HObj),
                Hist = db_obj:get_value(HistObj),
                Latest = case session_history:latest(Hist) of
                             {ok, S} ->
                                 S;
                             history_empty ->
                                 history_empty
                         end,
                session:stop(Latest),

                UserObj = session_history:resolve_conflict(
                            Hist, DbObj, #user.last_session),
                User = data_format:db_obj_to_rec(UserObj, ?USER_REC_NAME),

                case User#user.password == Login#user.password of
                    false ->
                        {error, invalid_login_data};
                    true ->
                        SessId = session:start(User, Hist, PushInfo),
                        NewHist = session_history:add(Hist, SessId),
                        NewHistObj = db_obj:set_value(HistObj, NewHist),
                        session_history:db_update(NewHistObj),
                        {ok, CheckHistObj} = session_history:db_get(Id),
                        case db_obj:has_siblings(CheckHistObj) of
                            true ->
                                stop_sessions(CheckHistObj),
                                {error, simultaneous_login};
                            false ->
                                NewUser = User#user{last_session = SessId},
                                NewUserPropList =
                                    data_format:rec_to_plist(NewUser),
                                NewUserObj =
                                    db_obj:set_value(UserObj, NewUserPropList),
                                db:put(NewUserObj, [{w, all}]),
                                {ok, SessId}
                        end
                end;
            {error, does_not_exist} ->
                {error, invalid_login_data};
            {error, Error} ->
                {error, Error}
        end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    ?DEBUG("Received unhandled call: ~p~n", [{_Request, _From, State}]),
    {noreply, ok, State}.


handle_cast({push_event, {UserId, Event}}, State) ->
    case session_presence:get_session_id(UserId) of
        {ok, SessionId} ->
            session:push_event(SessionId, Event);
        {error, not_online} ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    ?DEBUG("received unhandled cast: ~p~n",[{_Msg, State}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG("[~p] terminated ~p: reason: ~p, state: ~p ~n",
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
    (data_format:db_obj_to_rec(Obj, ?USER_REC_NAME))#user.id.

%% Stop the last session in all siblings.
stop_sessions(DbObj) ->
    Siblings = db_obj:get_siblings(DbObj),
    lists:foreach(fun(SibObj) ->
                          Hist = db_obj:get_value(SibObj),
                          Session = case session_history:latest(Hist) of
                                        {ok, S} ->
                                            S;
                                        history_empty ->
                                            history_empty
                                    end,
                          session:stop(Session)
                  end, Siblings),
    ok.
