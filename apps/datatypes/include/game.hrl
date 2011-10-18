-include_lib ("datatypes/include/date.hrl").

-type press () :: any ().
-type password () :: string () | undefined.

-record (game, {id :: integer (),
                creator_id :: integer (),
                name :: string (),
                description :: string (),
                status :: waiting | ongoing | stopped | finished,
                press :: press (),
                order_phase :: pos_integer (),
                retreat_phase :: pos_integer (),
                build_phase :: pos_integer (),
                num_players :: pos_integer (),
                password :: password (),
                result :: none | won | draw,
                waiting_time :: pos_integer (),
                date_created :: date (),
                date_completed :: date ()}).

-define (GAME_BUCKET, <<"game">>).

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
