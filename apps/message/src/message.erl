%%-------------------------------------------------------------------
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
%%% @doc
%%%   All interface to call message app
%%% @end
%%%
%%% @since : 15 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module (message).

-export ([
          user_msg/1,
          unread/1,
          mark_game_msg_as_read/1,
          mark_user_msg_as_read/1,
          game_msg/1,
          report_msg/1,
          mark_report_as_done/1,
          get_reports/1,
          get_all_game_msg/1,
          get_game_msg_by_phase/4
         ]).

-include_lib ("datatypes/include/game.hrl").
-include_lib("datatypes/include/message.hrl").
-include_lib("datatypes/include/bucket.hrl").

%% ------------------------------------------------------------------
%% Internal Macro Definitions
%% ------------------------------------------------------------------
-define(WORKER, message_worker).
-define(CAST_WORKER(Cmd), gen_server:cast(service_worker:select_pid(?WORKER), Cmd)).
-define(CALL_WORKER(Cmd), try gen_server:call(
                                service_worker:select_pid(?WORKER), Cmd)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).

%% ------------------------------------------------------------------
%% External API Function Definitions
%% ------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% @doc
%%  get the nick name of the recipient and message record which contain the
%%  sender id and content of message.
%% @end
%% -----------------------------------------------------------------------------
-spec user_msg(#message{}) ->
          {ok, MessageId :: integer()} |
          {error, nick_not_unique} |
          {error, send_msg_to_yourself} |
          {error, invalid_nick}|
          {error, black_listed}|
          {error, Error :: any()}.
user_msg(Msg=#message{}) ->
    ?CALL_WORKER({user_msg, Msg}).

%% --------------------------------------------------------------------
%% @doc
%%  Returns the list of unread messages where UserId is the recipient.
%% @end
%% --------------------------------------------------------------------
-spec message:unread(UserId :: integer()) -> {ok, {[#message{}],
                                                   [#game_message{}]}}.
unread(UserId) ->
    ?CALL_WORKER({unread, UserId}).

%% --------------------------------------------------------------------
%% @doc
%%  Marks the user message as read so that it will not be returned in
%%  subsequent calls to message:unread/1
%% @end
%% --------------------------------------------------------------------
-spec message:mark_user_msg_as_read(MessageId :: integer()) ->
          ok | {error, notfound}.
mark_user_msg_as_read(MessageId) ->
    ?CALL_WORKER({mark_as_read, MessageId, ?B_MESSAGE_UNREAD}).

%% --------------------------------------------------------------------
%% @doc
%%  Marks the game message as read so that it will not be returned in
%%  subsequent calls to message:unread/1
%% @end
%% --------------------------------------------------------------------
-spec message:mark_game_msg_as_read(MessageId :: integer()) ->
          ok | {error, notfound}.
mark_game_msg_as_read(MessageId) ->
    ?CALL_WORKER({mark_as_read, MessageId, ?B_GAME_MESSAGE_UNREAD}).

%% -----------------------------------------------------------------------------
%% @doc
%%  get a record of game message and log the message and send it to users
%% @end
%% -----------------------------------------------------------------------------
-spec game_msg(#game_message{}) -> ok.
game_msg(GMsg = #game_message{}) ->
    ?CAST_WORKER({game_msg, GMsg}).


%% -----------------------------------------------------------------------------
%% @doc
%%  log a report issue message
%% @end
%% -----------------------------------------------------------------------------
-spec report_msg(#report_message{}) -> ok.
report_msg(ReportMsg = #report_message{}) ->
    ?CALL_WORKER({report_msg, ReportMsg}).

%% -----------------------------------------------------------------------------
%% @doc
%%  mark a report as done
%% @end
%% -----------------------------------------------------------------------------
-spec mark_report_as_done(ReportId :: integer()) ->
                                 {ok, ReportId :: integer()} |
                                 {error, notfound}.
mark_report_as_done(ReportId) ->
    ?CALL_WORKER({mark_as_done, ReportId}).

%% -----------------------------------------------------------------------------
%% @doc
%%  gets all reports for a role not marked as done
%% @end
%% -----------------------------------------------------------------------------
-spec get_reports(Role :: atom()) -> {ok, [#report_message{}]}.
get_reports(Role) ->
    ?CALL_WORKER({get_reports, Role}).


get_all_game_msg(GameId) ->
    ?CALL_WORKER({get_all_game_msg, GameId}).


get_game_msg_by_phase(GameId, Year, Season, Phase)->
    ?CALL_WORKER({get_game_msg_by_phase, GameId, Year, Season, Phase}).
