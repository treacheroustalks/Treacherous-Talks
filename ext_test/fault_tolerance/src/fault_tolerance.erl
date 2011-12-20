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
            case eunit:test(backend_test:all_tests(), [verbose]) of
                ok ->
                    halt(0);
                Other ->
                    %% test/2 -> ok | {error, term()} isn't the whole story.
                    %% http://www.erlang.org/doc/man/eunit.html#test-1
                    %% Can also return 'error'.
                    io:format("eunit:test exited with ~p~n", [Other]),
                    halt(1)
            end
    end.
