-record (rule, {name,
                arity = 0 :: pos_integer () | all_orders,
                detector = fun (_Map,_OrdersAsTuple) -> true end,
                actor = fun (_Map, _OrdersAsTuple) -> ok end}).
