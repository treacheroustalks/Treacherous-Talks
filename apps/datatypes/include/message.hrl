-ifndef(MSG_DATAMODEL).
-define(MSG_DATAMODEL, true).

-include_lib("datatypes/include/date.hrl").

-record (message, {id :: integer(),
               from :: integer(),
               to :: integer(),
               content :: nonempty_string(),
               date_created :: date ()
               }).

-record (frontend_msg, {to :: nonempty_string(),
               content :: nonempty_string()
               }).

-endif.
