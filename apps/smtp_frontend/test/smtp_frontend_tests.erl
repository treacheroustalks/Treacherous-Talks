-module(smtp_frontend_tests).

-include_lib("eunit/include/eunit.hrl").
-export([smtp_frontend_test/0]).
smtp_frontend_test() ->
    %% replace lin.pcs with your domain name
    gen_smtp_server:start(smtp_core,[[{port, 25}, {domain, "lin.pcs"}]]),

    %% send message via lin.pcs to echo server smtp_echo.pcs(which runs on server3)
    %% make sure lin_vm1.pcs is running and ECHO mode is on
    gen_smtp_client:send({"mike@lin.pcs",
                         ["joe@smtp_echo.pcs"],
                         "hello mike\r\nhello joe\r\nhello robert"},
                         [{relay, "lin.pcs"}, {port, 25}]),
    ok.

% if success, it will print out echo message "hello mike\r\nhello joe\r\nhello robert" in our terminal
