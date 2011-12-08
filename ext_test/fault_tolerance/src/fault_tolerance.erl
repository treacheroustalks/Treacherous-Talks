-module(fault_tolerance).

-export([main/1]).

main(Args) ->
    [ClustConfPath] = Args,
    case file:consult(ClustConfPath) of
        {error, Reason} ->
            ErrorString = file:format_error(Reason),
            io:format(standard_error, "~n~s: ~s~n~n", [ErrorString, ClustConfPath]),
            halt(1);
        {ok, [Config]} ->
            application:set_env(fault_tolerance, test_cluster_config, Config),
            eunit:test(backend_test)
    end.
