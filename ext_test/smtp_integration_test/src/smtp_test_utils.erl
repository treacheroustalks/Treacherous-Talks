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
%%%
%%% Utilities for SMTP Frontend integration tests
%%%
%%%------------------------------------------------------------------
-module(smtp_test_utils).

-export([send_mail/1,
         from_address/0,
         to_address/0,
         get_hostname/0]).

%%------------------------------------------------------------------
%% Send the content to a locally-running SMTP Frontend on port 2525.
%% The mail will be from test@<your hostname> e.g. test@dilshod.pcs
%% The mail will be to mailbot@<your hostname>
%%------------------------------------------------------------------
send_mail(Content) ->
    gen_smtp_client:send_blocking({from_address(),
                                   [to_address()],
                                   Content
                                  },
                                  [
                                   {relay, get_hostname()},
                                   {port, 2525},
                                   {tls, never}
                                  ]
                                 ).

%%------------------------------------------------------------------
%% Construct a sender address for the test user
%%------------------------------------------------------------------
from_address() ->
    SenderAddress = "test@"++get_hostname(),
    SenderAddress.

%%------------------------------------------------------------------
%% Construct the address to mail to at the local SMTP Frontend
%%------------------------------------------------------------------
to_address() ->
    RecipientAddress = "mailbot@"++get_hostname(),
    RecipientAddress.

%%------------------------------------------------------------------
%% Find the full hostname of the current machine.
%%------------------------------------------------------------------
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    {ok,{hostent,FullHostname,[],inet,_,[_]}} = inet:gethostbyname(Hostname),
    FullHostname.
