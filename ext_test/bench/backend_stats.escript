#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name backend_stats@andre.pcs -mnesia debug verbose -setcookie treacherous_talks
-module(backend_stats.escript).


main([Backend]) when is_list(Backend) -> 
    Node = list_to_atom(Backend),
    pong = net_adm:ping(Node),
    {ok, Result} = rpc:call(Node, system_stats, get_system_stats, [string]),
    {ok, DbResult} = rpc:call(Node, system_stats, get_db_stats, []),
    io:format("~s~n", [Result]),
    lists:foreach(fun({ok, DbNode}) ->
                          io:format("~s~n", [DbNode])
                  end, DbResults);
main(_) ->
    io:format("You need to supply a backend node name").
