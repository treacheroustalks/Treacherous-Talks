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
-export([has_access/2]).

% include files
-include_lib("datatypes/include/user.hrl").

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%%   this function get a command and the user role and return ture or false
%%
%%   Note: whenever a new command add to controller we need to update
%%    both tt_acl:moderator_cmd() and tt_acl:user_cmd
%% @end
%%-------------------------------------------------------------------
-spec has_access(atom(), role()) -> boolean().
has_access(_Command, operator) ->
    true;
has_access(Command, moderator) ->
    case lists:member(Command, moderator_cmd()) of
        true ->
            true;
        false ->
            lists:member(Command, user_cmd())
    end;
has_access(Command, user) ->
    lists:member(Command, user_cmd()).

%% ------------------------------------------------------------------
%% Internal function definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc
%%   return a list of cammands that users with moderator role has access
%% @end
%% ------------------------------------------------------------------
moderator_cmd() ->
    [power_msg,
     get_reports,
     mark_report_as_done
    ].

%% ------------------------------------------------------------------
%% @doc
%%   return a list of cammands that users with user role has access
%% @end
%% ------------------------------------------------------------------
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
     send_report
    ].
