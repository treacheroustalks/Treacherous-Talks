-module(service_worker).

-export([join_group/1, select_pid/1]).

join_group(Name) ->
    pg2:create(Name),
    pg2:join(Name, self()).

select_pid(Name) ->
    pg2:get_closest_pid(Name).
