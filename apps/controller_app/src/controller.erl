%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author Tiina Loukusa <loukusa@gmail.com>
%%%
%%% @doc Provides an API for the Treacherous Talks frontend
%%% @end
%%%
%%% @since : 17 Oct 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(controller).

%% Public API
-export([handle_action/2]).


%% Internal functions, exported for eUnit, do not use!
-export([
         register/1,
         login/1,
         get_user/2
        ]).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

-define(WORKER, controller_app_worker).
-define(CALL_WORKER(Cmd), gen_server:call(service_worker:select_pid(?WORKER),
                                          Cmd)).

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
       Command == game_order->
    case session:alive(SessionId) of
        false ->
            CallbackFun(Args, {Command, invalid_session}, SessionId);
        true->
            case session:Command(SessionId, Data) of
                {error, Error} ->
                    CallbackFun(Args, {Command, invalid_data}, Error);
                {ok, Result} ->
                    CallbackFun(Args, {Command, success}, Result)
            end
    end;

handle_action({Command, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {Command, parse_error}, Error);

handle_action(unknown_command, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, []);
handle_action(Cmd, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, Cmd).


%%-------------------------------------------------------------------
%% @doc create_user/2
%%
%% API for creation of a user
%% @end
%% [@spec create_user(Id::Integer(), #user{}) @end]
%%-------------------------------------------------------------------
register(User) ->
    ?CALL_WORKER({register, User}).


%%-------------------------------------------------------------------
%% @doc login/1
%%
%% API for logging in a user
%% @end
%% [@spec login(#user{}) @end]
%%-------------------------------------------------------------------
login(User) ->
    ?CALL_WORKER({login, User}).


%%-------------------------------------------------------------------
%% @doc get_user/2
%%
%% API for getting a user
%% @end
%% [@spec get_user(integer()|string()) @end]
%%-------------------------------------------------------------------
get_user(Type, Key) ->
    ?CALL_WORKER({get_user, Type, Key}).
