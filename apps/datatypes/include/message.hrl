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
-ifndef(MSG_DATAMODEL).
-define(MSG_DATAMODEL, true).

-include_lib("datatypes/include/date.hrl").
-include_lib("datatypes/include/game.hrl").

-type report_type() :: report_player | report_problem.
-type power_user() :: moderator | operator.

-record(unread, {id :: integer()
                       }).

-record (message, {id :: integer(),
                   from_id :: integer(),
                   from_nick :: string (),
                   to_id :: integer(),
                   to_nick :: string(),
                   content :: nonempty_string(),
                   date_created :: date ()
                  }).

-record (game_message, {id :: integer(),
                        game_id :: integer(),
                        from_id :: integer(),
                        from_country :: country()| unknown,
                        sender_country :: country(),% for operator to see grey press
                        group_id :: integer(),% to merge broadcast messages into one
                        to_id :: integer(),
                        to_country :: country(),
                        content :: nonempty_string(),
                        date_created :: date (),
                        year :: integer(),
                        season :: spring | fall,
                        phase :: phase()
                       }).

-record (report_message, {id :: integer(),
                          from_id :: integer(),
                          from_nick = undefined :: undefined | string(),
                          to :: power_user(),
                          type :: report_type(),
                          content :: nonempty_string(),
                          date_created :: date()
                         }).

%% The "to" field in record frontend_msg:
%% * For off-game communication it's a string()
%% * For in-game communication it's a list(), so that we can send to
%%   several players  e.g. [england, russia, french]
-record (frontend_msg, {to :: list(atom()) | string(),
                        content :: nonempty_string(),
                        game_id = undefined :: undefined | integer()
                       }).

-endif.
