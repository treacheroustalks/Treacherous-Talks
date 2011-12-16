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
%%% @doc
%%% All the bucket names are to be included in this file
%%% @end
%%%
%%% @since : 18 Oct 2011 by Bermuda Triangle
%%% @end
%%%
%%%-------------------------------------------------------------------

% User information
-define(B_USER, <<"user">>).

% User session history
-define(B_SESSION_HISTORY, <<"session_history">>).


% Game information
-define(B_GAME, <<"game">>).

% Information of the current phase of a specific game
-define(B_GAME_CURRENT, <<"game_current">>).

% Mapping from game to game players
-define(B_GAME_PLAYER, <<"game_player">>).

% Game orders (for a specific phase)
-define(B_GAME_ORDER, <<"game_order">>).

% Game state (for a specific phase)
-define(B_GAME_STATE, <<"game_state">>).


% General messages
-define(B_MESSAGE, <<"message">>).
-define(B_MESSAGE_UNREAD, <<"message_unread">>).

% In game messages
-define(B_GAME_MESSAGE, <<"game_message">>).
-define(B_GAME_MESSAGE_UNREAD, <<"game_message_unread">>).

% Report messages
-define(B_REPORT_MESSAGE, <<"report_message">>).
-define(B_REPORT_MESSAGE_UNREAD, <<"report_message_unread">>).

% Corpse information
-define(B_CORPSES, <<"corpses">>).
