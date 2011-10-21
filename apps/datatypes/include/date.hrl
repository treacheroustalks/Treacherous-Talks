-ifndef(DATE).

-type date() :: {{integer (), integer (), integer ()},
                 {integer (), integer (), integer ()}}.

-define (DATE(), {date (), now ()).


-endif.