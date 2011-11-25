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
%%% @doc Unit tests for the tt_acl interface.
%%% @end
%%%
%%% @since : 25 Nov 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(tt_acl_test).

-include_lib("eunit/include/eunit.hrl").



set_up() ->
    ?debugMsg("tt_acl test start ...").
teardown(_) ->
    ?debugMsg("tt_acl test DONE").

tt_acl_test_ () ->
    {setup,
     fun set_up/0,
     fun teardown/1,
     has_access_tst_()
    }.

%% testing the tt_acl interface
has_access_tst_() ->
    [{"test the tt_acl interface for oprator",
      fun() ->
              ?assertEqual(true,
                           tt_acl:has_access(update_user, operator))
      end},
     {"test the tt_acl interface for normal user",
      fun() ->
              ?assertEqual(true,
                           tt_acl:has_access(update_user, user))
      end}
    ].