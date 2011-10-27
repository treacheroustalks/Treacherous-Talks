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
-export([create_user/1,
         get_user/1, get_user/2,
         update_user/1,
         login_user/1,
         new_game/1,
         reconfig_game/1,
         get_game/1,
         update_rec_by_proplist/2]).

-include_lib("datatypes/include/user.hrl").
-include_lib("datatypes/include/game.hrl").

-define(WORKER, controller_app_worker).

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
%% CallbackFun(Args, {Type::command(), Result::result()}, ResultData::any()) -> ok.
%%
%% command() :: register |
%%              login |
%%              create_user |
%%              update_user |
%%              unkown_command.
%%
%% result() :: success | parse_error | invalid_data.
%% @end
%%
%% [@spec
%% handle_action(ParsedData::{command(), any()}, {CallbackFun::Fun, Args::[any()]) -> ok.
%% @end]
%%-------------------------------------------------------------------
handle_action({register, {ok, UserInfo}}, {CallbackFun, Args}) ->
    case controller:create_user(UserInfo) of
        error ->
            CallbackFun(Args, {register, invalid_data}, error);
        UserRec ->
            CallbackFun(Args, {register, success}, UserRec)
    end;
handle_action({register, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {register, parse_error}, Error);

handle_action({login, {ok, UserInfo}}, {CallbackFun, Args}) ->
    case controller:login_user(UserInfo) of
        invalid ->
            CallbackFun(Args, {login, invalid_data}, UserInfo);
        Session ->
            CallbackFun(Args, {login, success}, Session)
    end;
handle_action({login, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {login, parse_error}, Error);

handle_action({update_user, {ok, Nick, UpdateUserProplist}}, {CallbackFun, Args}) ->
    case controller:get_user(#user.nick, Nick) of
        [] ->
            CallbackFun(Args, {update_user, invalid_data}, UpdateUserProplist);
        [OldUser | _] ->
            UpdatedUser = update_rec_by_proplist(OldUser, UpdateUserProplist),
            UserRec = controller:update_user(UpdatedUser),
            CallbackFun(Args, {update_user, success}, UserRec)
    end;
handle_action({update_user, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {update_user, parse_error}, Error);

handle_action({reconfig_game, {ok, GameId, GamePropList}}, {CallbackFun, Args}) ->
    case controller:get_game(GameId) of
        {ok, #game{status = waiting} = OldGame} ->
            % it is only possible to update when status is waiting
            NewGame = update_rec_by_proplist(OldGame, GamePropList),
            {ok, _Id} = controller:reconfig_game(NewGame),
            CallbackFun(Args, {reconfig_game, success}, NewGame);
        _ ->
            CallbackFun(Args, {reconfig_game, invalid_data}, GamePropList)
    end;
handle_action({reconfig_game, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {reconfig_game, parse_error}, Error);

handle_action({create_game, {ok, GameInfo}}, {CallbackFun, Args}) ->
    GameRec = controller:new_game(GameInfo),
    % @TODO no invalid register create_game case yet ?
    CallbackFun(Args, {create_game, success}, GameRec);
handle_action({create_game, Error}, {CallbackFun, Args}) ->
    CallbackFun(Args, {create_game, parse_error}, Error);

handle_action(unknown_command, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, []);
handle_action(Cmd, {CallbackFun, Args}) ->
    CallbackFun(Args, unknown_command, Cmd).


%%------------------------------------------------------------------------------
%% @doc update record via a proplist
%%  Input: Arg1: OldUser#user
%%         Arg2: [{#user.name, "username"}, {#user.password, "xxxx"}]
%%
%%  Output: #user{name="username", password="xxxx"}
%% @end
%%------------------------------------------------------------------------------
update_rec_by_proplist(OldUser, [{_, field_missing}|Rest]) ->
    update_rec_by_proplist(OldUser, Rest);
update_rec_by_proplist(OldUser, [{Field, Value}|Rest]) ->
    update_rec_by_proplist(setelement(Field, OldUser, Value), Rest);
update_rec_by_proplist(UpdatedUser, []) ->
    UpdatedUser.


%%-------------------------------------------------------------------
%% @doc create_user/2
%%
%% API for creation of a user
%% @end
%% [@spec create_user(Id::Integer(), #user{}) @end]
%%-------------------------------------------------------------------
create_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {create_user, User}).


%%-------------------------------------------------------------------
%% @doc update_user/1
%%
%% API for updating a user
%% @end
%% [@spec create_user(#user{}) @end]
%%-------------------------------------------------------------------
update_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {update_user, User}).


%%-------------------------------------------------------------------
%% @doc login_user/1
%%
%% API for logging in a user
%% @end
%% [@spec login_user(#user{}) @end]
%%-------------------------------------------------------------------
login_user(User) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {login_user, User}).


%%-------------------------------------------------------------------
%% @doc get_user/2
%%
%% API for getting a user
%% @end
%% [@spec get_user(integer()|string()) @end]
%%-------------------------------------------------------------------
get_user(Type, Key) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                   {get_user, Type, Key}).
get_user(Id) ->
    io:format ("get_user/2 :)~n"),
    gen_server:call(service_worker:select_pid(?WORKER),
                    {get_user, Id}).


%%-------------------------------------------------------------------
%% @doc
%% API for creation of a game
%% @end
%% [@spec create_game(#game{}) @end]
%%-------------------------------------------------------------------
new_game(Game) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {new_game, Game}).

%%-------------------------------------------------------------------
%% @doc
%% API for updating a game
%% @end
%% [@spec reconfig_game(#game{}) @end]
%%-------------------------------------------------------------------
reconfig_game(Game) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {reconfig_game, Game}).

%%-------------------------------------------------------------------
%% @doc
%% API for getting a game
%% @end
%% [@spec get_game(Id::Integer()) @end]
%%-------------------------------------------------------------------
get_game(Id) ->
    gen_server:call(service_worker:select_pid(?WORKER),
                    {get_game, Id}).
