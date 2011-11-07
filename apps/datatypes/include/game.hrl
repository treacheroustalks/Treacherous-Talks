-ifndef(GAME_DATAMODEL).
-define(GAME_DATAMODEL, true).

-include_lib("datatypes/include/date.hrl").

-type press () :: any ().
-type password () :: string () | undefined.
-type country () :: england |
                    germany |
                    france |
                    austria |
                    italy |
                    russia |
                    turkey.
-type phase () ::  order | build | retreat.

-define (REQUIRED (Field), Field = erlang:error ({error, {field_required,Field, ?MODULE,?LINE}})).
-define (REQUIRED, erlang:error ({error, {field_requried, ?MODULE,?LINE}})).
%-define (REQUIRED, undefined).
-define(GAME_PLAYER_LINK_USER, <<"user">>).

-record (game, {id :: integer (),
                creator_id :: integer (),
                ?REQUIRED (name) :: string (),
                description :: string (),
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
                date_completed = undefined :: date (),
                last_session :: string()
               }).


-record (game_user, {
    id :: integer (),
    country :: country (),
    last_game_order_id :: integer()}).

-record (game_player, {
    id :: integer (),
    players = [] :: [#game_user{}]}).

-record (game_state, {
    id :: integer(),
    year_season :: any(),
    phase :: phase(),
    map :: any()}).

-record(game_overview, {
    game_rec :: #game{},
    country :: country(),
    map :: any()}).

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
%INDEX
%status
%press
%num_players

%BUCKET
%game

%KEY
%id

%LINK
%game_message    the game links to all the messages for the current game
