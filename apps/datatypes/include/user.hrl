-include_lib ("datatypes/include/date.hrl").

-record (user, {id :: integer(),
                nick :: nonempty_string (),
                email :: nonempty_string (),
                password :: nonempty_string (),
                name :: nonempty_string (),
                role = user :: atom (),
                channel :: atom (),
                last_ip :: {integer (), integer (), integer (), integer ()},
                last_login :: date () | never,
                score = 0 :: integer (),
                date_created :: date (),
                date_updated :: date ()
               }).

