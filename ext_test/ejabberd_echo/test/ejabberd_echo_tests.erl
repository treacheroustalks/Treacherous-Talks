-module(ejabberd_echo_tests).

-include_lib("eunit/include/eunit.hrl").

-record(state, {session,msg}).

ejabberd_echo_test() ->
    xmpp_client:start_link(),
    ok.

