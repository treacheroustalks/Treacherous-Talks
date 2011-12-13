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
-ifndef(GAME_DATAMODEL).
-define(GAME_DATAMODEL, true).

-include_lib("datatypes/include/date.hrl").

-type press () :: white | grey | none.
-type password () :: string () | undefined.
-type country () :: england |
                    germany |
                    france |
                    austria |
                    italy |
                    russia |
                    turkey.
-type phase () ::  order_phase | build_phase | retreat_phase.
-type season() :: spring | fall.

-define (REQUIRED (Field), Field = erlang:error ({error, {field_required,Field, ?MODULE,?LINE}})).
-define (REQUIRED, erlang:error ({error, {field_requried, ?MODULE,?LINE}})).
-define(GAME_PLAYER_LINK_USER, <<"user">>).
-define(GAME_STATE_LINK_GAME, <<"game">>).
-define(CURRENT_GAME_LINK_STATE, <<"game_state">>).
-define(GAME_REC_NAME, game).
-define(GAME_MSG_REC_NAME, game_message).

-record (game, {id :: integer (),
                creator_id :: integer (),
                ?REQUIRED (name) :: string (),
                description = "" :: string (),
                status = waiting :: waiting | ongoing | stopped | finished,
                ?REQUIRED (press) :: press (),
                ?REQUIRED (order_phase) :: pos_integer (),
                ?REQUIRED (retreat_phase) :: pos_integer (),
                ?REQUIRED (build_phase) :: pos_integer (),
                num_players = 0 :: pos_integer (),
                password = undefined :: password (),
                result = none :: none | won | draw,
                ?REQUIRED (waiting_time) :: pos_integer (),
                date_created = {date (), time ()} :: date (),
                date_completed = undefined :: undefined | date (),
                start_time = undefined :: undefined | {pos_integer(), pos_integer(), pos_integer()},
                last_session = "" :: string()
               }).


-record (game_user, {
    id :: integer (),
    country :: country ()}).

-record (game_player, {
    id :: integer (),
    players = [] :: [#game_user{}]}).

-record (game_state, {
    id :: integer(),
    year_season :: any(),
    phase :: phase(),
    map :: any()}).

-record(game_current, {
          id :: integer(),
          year_season :: {pos_integer(), season()},
          current_phase :: phase(),
          date_updated = {date (), time ()} :: date()}).

-record(game_overview, {
    game_rec :: #game{},
    phase :: phase(),
    year_season :: {pos_integer(), season()},
    country :: country(),
    players :: list(),
    map :: any(),
    order_list :: any()}).

-record(game_order, {
    id :: integer(),
    game_id :: integer(),
    user_id :: integer(),
    country :: country(),
    year_season :: any(),
    phase :: phase(),
    date_created = {date (), time ()} :: date (),
    order_list :: any()}).

-endif.
