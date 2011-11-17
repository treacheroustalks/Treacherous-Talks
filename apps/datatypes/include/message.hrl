-ifndef(MSG_DATAMODEL).
-define(MSG_DATAMODEL, true).

-include_lib("datatypes/include/date.hrl").

-record (message, {id :: integer(),
                   from_id :: integer(),
                   from_nick :: string (),
                   to_id :: integer(),
                   to_nick :: string(),
                   content :: nonempty_string(),
                   date_created :: date (),
                   status = unread :: read | unread
               }).

-record (frontend_msg, {to :: nonempty_string(),
               content :: nonempty_string()
               }).

-endif.
