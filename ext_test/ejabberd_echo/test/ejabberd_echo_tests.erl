-module(ejabberd_echo_tests).

-include_lib("eunit/include/eunit.hrl").

ejabberd_echo_test() ->
    xmpp_client:start_link(),
    ok.

