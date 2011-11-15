%%%-------------------------------------------------------------------
%%% @copyright
%%% Copyright (C) 2011 by Bermuda Triangle
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
-module(smtp_frontend_tests).

-include_lib("eunit/include/eunit.hrl").

%For manual test
-export([smtp_frontend_testcase1/0]).

smtp_frontend_testcase1() ->
    %% replace lin.pcs with your domain name
    gen_smtp_server:start(smtp_core,[[{port, 25}, {domain, "lin.pcs"}]]),

    %% send message via lin.pcs to echo server smtp_echo.pcs(which runs on server3)
    %% make sure lin_vm1.pcs is running and ECHO mode is on
    gen_smtp_client:send({"mike@lin.pcs",
                         ["joe@smtp_echo.pcs"],
                         "hello mike\r\nhello joe\r\nhello robert"},
                         [{relay, "lin.pcs"}, {port, 25}]).

% if success, it will print out echo message "hello mike\r\nhello joe\r\nhello robert" in our terminal
