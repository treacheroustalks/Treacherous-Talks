-record (rule, {name,
                arity,
                detector = fun (_Map,_OrdersAsTuple) -> true end,
                actor = fun (_Map, _OrdersAsTuple) -> ok end}).
