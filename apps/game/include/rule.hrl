-record (rule, {name,
                arity,
                detector = fun (_Map,_OrdersAsTuple) -> false end, 
                actor = fun (_OrdersAsTuple) -> ok end}).
