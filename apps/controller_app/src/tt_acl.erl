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
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc Provid functionality to check whether uer has access to command or not
%%% @end
%%%
%%% @since : 25 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(tt_acl).


%% Public API
-export([
         has_access/2, has_access/3
        ]).

% include files
-include_lib("datatypes/include/user.hrl").

%%-------------------------------------------------------------------
%% @doc
%%   Checks if the given user has access to specified command
%%
%%   Note: whenever a new command add to controller we need to update
%%   both tt_acl:moderator_cmd() and tt_acl:user_cmd
%% @end
%%-------------------------------------------------------------------
-spec has_access(atom(), string()) -> {Status::atom(), Role::atom()}.
has_access(Command, SessionId) ->
    check_access(Command, SessionId).

%%-------------------------------------------------------------------
%% @doc
%%   Checks if the given user has access to specified command
%%
%% @end
%%-------------------------------------------------------------------
-spec has_access(atom(), string(), term()) -> {Status::atom(), Role::atom()}.
has_access(Command, Role, Data) ->
    case check_access(Command, Role) of
        true ->
            % Check for special cases
            case Command of
                blacklist ->
                    check_blacklist_access(Role, Data);
                whitelist ->
                    check_blacklist_access(Role, Data);
                _ ->
                    true
            end;
        false ->
            false
    end.

%% ------------------------------------------------------------------
%% Internal function definitions
%% ------------------------------------------------------------------

%% Check of the user's role has access to specified command
check_access(Command, Role) ->
    case Role of
        user ->
            lists:member(Command, user_cmd());
        moderator ->
            lists:member(Command, user_cmd()) orelse
                lists:member(Command, moderator_cmd());
        disabled ->
            false;
        operator ->
            true;
        false ->
            false
    end.

%% Check if specified role has permission to blacklist given user
% Operators can blacklist user with any role
% Moderators can blacklist user with "user" role
check_blacklist_access(Role, TargetNick) ->
    case user_management:get(#user.nick, TargetNick) of
        {ok, #user{role = TargetRole}} ->
            case {Role, TargetRole} of
                {operator, _} ->
                    true;
                {moderator, user} ->
                    true;
                {moderator, disabled} ->
                    true;
                _ ->
                    false
            end;
        _Error ->
            false
    end.

%% List of commands the moderator has access to
moderator_cmd() ->
    [power_msg,
     get_reports,
     mark_report_as_done,
     blacklist,
     whitelist
    ].

%% List of commands the user has access to
user_cmd() ->
    [update_user,
     get_session_user,
     create_game,
     get_game,
     reconfig_game,
     game_overview,
     join_game,
     game_order,
     logout,
     user_msg,
     games_current,
     game_search ,
     game_msg,
     get_presence,
     send_report,
     set_push_receiver
    ].
