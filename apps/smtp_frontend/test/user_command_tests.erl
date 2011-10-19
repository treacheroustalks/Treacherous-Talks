-module(user_command_tests).

-include_lib("eunit/include/eunit.hrl").
-import(user_command, [get_reg_info/1, reg_info_refine/1, new_user_record/1]).

% Export for manual test
-export([display/0]).

-include_lib("../../smtp_frontend/include/test_utils.hrl").


% EUnit auto test------------------------------------------------------
get_reg_info_test_() ->
    Output = {ok,{reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>}},
    ?_assert(get_reg_info(?SAMPLE_EMAIL) == Output).

reg_info_refine_test_() ->
    Raw = [<<"NICKNAME: Lin">>,<<"PASSWORD: QWER">>,<<>>,<<"EMAIL: ss@pcs">>,
           <<"asdfasdlfadf">>,<<>>,<<"FULLNAME: Agner Erlang">>],
    Output = {ok,{reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>}},
    ?_assert(reg_info_refine(Raw) == Output).

new_user_record_test_() ->
    Input = {reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>},
    ExpectedOutput = {user,undefined,<<"Lin">>,<<"ss@pcs">>,<<"QWER">>,<<"Agner Erlang">>,
                        user,undefined,undefined,undefined,0,undefined,undefined},
    ?_assert(new_user_record(Input) == ExpectedOutput).

% Manual test------------------------------------------------------
pnt(M) -> io:format("####~p~n-----------------------------~n", [M]).
pnt(L,M) -> io:format("####~p:~p~n-----------------------------~n", [L, M]).

display()->
    pnt(get_reg_info(?SAMPLE_EMAIL)),
    BinStr = ?SAMPLE_EMAIL,

    pnt(?LINE,"pnt ok"),

    HeadPos = binary:match(BinStr, <<"REGISTER\r\n">>),
    HeadCut = bin_utils:tailstr(BinStr, HeadPos),
    TailPos = binary:match(HeadCut, <<"\r\nEND">>),
    TailCut = bin_utils:headstr(HeadCut, TailPos),
    pnt(?LINE,TailCut),
    RawInfoList = binary:split(TailCut, <<"\r\n">>, [global, trim]),
    pnt(?LINE,RawInfoList),
    pnt(?LINE,reg_info_refine(RawInfoList)),
    pnt(?LINE,get_reg_info(BinStr)).
