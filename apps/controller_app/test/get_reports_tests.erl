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
%%% @doc Get reports tests for operator
%%% @end
%%%
%%% @since : 6 Dec 2011 by Bermuda Triangle
%%% @end
%%%-------------------------------------------------------------------
-module(get_reports_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/message.hrl").

-export([tests/1, success/2, invalid/2]).

tests([Callback, SessId]) ->
    [
     ?_test(success(Callback, SessId)),
     ?_test(invalid(Callback, SessId))
    ].
%%-------------------------------------------------------------------
%% Operator get db status test
%%-------------------------------------------------------------------
success(Callback, SessId) ->
    ?debugMsg("GET REPORTS TEST SUCCESS"),
    Content = db_c:get_unique_id(),
    send_report(SessId, Callback, Content),
    timer:sleep(100),

    Cmd = {get_reports, {ok, SessId, []}},
    Result = controller:handle_action(Cmd, Callback),
    {CmdRes, List} = Result,
    ?assertEqual({get_reports, success}, CmdRes),

    ContentList = get_content(List),
    ?assert(lists:keymember(Content, 1, ContentList)),
    {Content, Id} = lists:keyfind(Content, 1, ContentList),
    ?debugMsg("GET REPORTS TEST SUCCESS finished"),

    ?debugMsg("MARK REPORT AS DONE"),
    Cmd2 = {mark_report_as_done, {ok, SessId, Id}},
    Result2 = controller:handle_action(Cmd2, Callback),
    {CmdRes2, _Data} = Result2,
    ?assertEqual({mark_report_as_done, success}, CmdRes2),
    timer:sleep(100),

    % get reports and assert that the report is not included
    Cmd3 = {get_reports, {ok, SessId, []}},
    Result3 = controller:handle_action(Cmd3, Callback),
    {_CmdRes3, List2} = Result3,
    ContentList3 = get_content(List2),    
    ?assertNot(lists:keymember(Content, 1, ContentList3)),
    ?debugMsg("MARK REPORT AS DONE finished").

invalid(_Callback, _SessId) ->
    ok.

send_report(SessId, Callback, Content) ->
    Data = get_test_data(Content),
    Cmd = {send_report, {ok, SessId, Data}},
    controller:handle_action(Cmd, Callback).

get_test_data(Content) ->
    #report_message{to = operator,
                    type = report_player,
                    content = Content}.

get_content(List) ->
    lists:map(fun(#report_message{id = Id, content = Content}) ->
                      {Content, Id}
              end, List).
