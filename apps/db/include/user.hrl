-type date() :: {{integer (), integer (), integer ()},
                 {integer (), integer (), integer ()}}.

-record (user, {email :: nonempty_string (),
                password :: nonempty_string (),
                name :: nonempty_string (),
                role = user :: atom (),
                channel :: atom (),
                last_ip :: {integer (), integer (), integer (), integer ()},
                last_login :: date (),
                score = 0 :: integer (),
                date_created :: date (),
                date_updated :: date ()
               }).

