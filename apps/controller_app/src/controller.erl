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
%%% @doc Provides an API for the Treacherous Talks frontend
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @todo broken specs
%%% @end
%%%-------------------------------------------------------------------
-module(controller).

%% Public API
-export([handle_action/2, push_event/2]).


%% Internal functions, exported for eUnit, do not use!
-export([
         register/1,
         login/1
        ]).

-include_lib("datatypes/include/push_receiver.hrl").
-include_lib("datatypes/include/push_event.hrl").
-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

%% ------------------------------------------------------------------
%% Internal macros
%% ------------------------------------------------------------------
-define(WORKER, controller_app_worker).
-define(SELECT_WORKER, service_worker:select_pid(?WORKER)).
-define(CAST_WORKER(Cmd), gen_server:cast(?SELECT_WORKER, Cmd)).
-define(CALL_WORKER(Cmd), gen_server:call(?SELECT_WORKER, Cmd)).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Main controller function. Expects a command from the frontend,
%% a callback function, and its arguments. The callback is called
%% with the results of the given command. The callback function
%% must be of arity 3:
%%
%% CallbackFun(Args, {Type::command(), Result::result()},
%%                    ResultData::any()) -> ok.
%%
%% command() :: register |
%%              login |
%%              get_session_user |
%%              update_user |
%%              create_game |
%%              get_game |
%%              reconfig_game |
%%              game_order |
%%              join_game |
%%              game_overview |
%%              games_current |
%%              user_msg |
%%              game_msg |
%%              unkown_command.
%%
%% result() :: success | parse_error | invalid_data | invalid_session | error.
%%
%% Standard return (error) values for invalid_data
%% register ->          [nick_already_exists]
%% login ->             [nick_not_unique,
%%                       invalid_login_data,
%%                       simultaneous_login]
%% get_session_user ->  []
%% update_user ->       [does_not_exist]
%% create_game ->       []
%% get_game ->          [game_does_not_exist]
%% reconfig_game ->     [game_does_not_exist,
%%                       game_started_already,
%%                       not_game_creator]
%% game_order ->         [game_id_not_exist,
%%                       user_not_playing_this_game,
%%                       game_not_waiting]
%% join_game ->         [country_not_available,
%%                       user_already_joined]
%% game_overview ->     [user_not_playing_this_game]
%% logout ->            []
%% user_msg ->          [nick_not_unique,
%%                       invalid_nick]
%% games_current ->     []
%% game_search ->       []
%% game_msg ->          [not_allowed_send_msg,
%%                      game_does_not_exist,
%%                      game_phase_not_ongoing]
%%
%% @end
%%
%% [@spec
%% handle_action(ParsedData::{command(), {ok, any()}} |
%%                           {command(), {ok, integer(), any()}} |
%%                           {command(), {error, any()}},
%%               {CallbackFun::Fun, Args::[any()]},
%%               SessionId::Integer()) -> ok.
%% @end]
%%
%%   Note: whenever a new command add to controller we need to update
%%    both tt_acl:moderator_cmd() and tt_acl:user_cmd
%%-------------------------------------------------------------------
handle_action({Command, {ok, Data}}, {CallbackFun, Args})
  when Command == register;
       Command == login ->
    case controller:Command(Data) of
        {error, Error} ->
            CallbackFun(Args, {Command, invalid_data}, Error);
        {ok, Result} ->
            CallbackFun(Args, {Command, success}, Result)
    end;
handle_action({Command, {ok, SessionId, Data}}, {CallbackFun, Args})
  when Command == update_user;
       Command == get_session_user;
       Command == create_game;
       Command == get_game;
       Command == reconfig_game;
       Command == game_overview;
       Command == join_game;
       Command == game_order;
       Command == logout;
       Command == user_msg;
       Command == games_current;
       Command == game_search ;
       Command == game_msg ->
    case session:alive(SessionId) of
        false ->
            CallbackFun(Args, {Command, invalid_session}, SessionId);
        true->
            {ok, #user{role = Role}} = session:get_session_user(SessionId, user),
            case tt_acl:has_access(Command, Role) of
                false ->
                    CallbackFun(Args, {Command, access_denied}, Role);
                true ->
                    case session:Command(SessionId, Data) of
                        {error, Error} ->
                            CallbackFun(Args, {Command, invalid_data}, Error);
                        {ok, Result} ->
                            CallbackFun(Args, {Command, success}, Result)
                    end
            end
    end;
handle_action({Command, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {Command, parse_error}, Error);

handle_action(unknown_command, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, []);
handle_action(Cmd, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, Cmd).


%%-------------------------------------------------------------------
%% @doc
%% Pushes an event to the user with given id, if online.
%%
%% @spec push_event(UserId::integer(), #push_event{}) -> ok
%% @end
%%-------------------------------------------------------------------
push_event(UserId, Event = #push_event{}) ->
    ?CAST_WORKER({push_event, {UserId, Event}}).


%%-------------------------------------------------------------------
%% @todo the whole documentation here is wrong. not only arity, even the name
%% @deprecated only for eunit
%% @doc create_user/2
%%
%% API for creation of a user
%% @end
%% [@spec create_user(Id::Integer(), #user{}) @end]
%%-------------------------------------------------------------------
register(User) ->
    ?CALL_WORKER({register, User}).


%%-------------------------------------------------------------------
%% @deprecated only for eunit
%% @doc login/1
%%
%% API for logging in a user
%%
%% @spec login({#user{}, #push_receiver{}}) ->
%%          {ok, SessionId} | {error, nick_not_unique} |
%%          {error, invalid_login_data} | {error, simultaneous_login}
%% @end
%%-------------------------------------------------------------------
login(Data = {#user{}, #push_receiver{}}) ->
    ?CALL_WORKER({login, Data}).
