-module(user_command_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../smtp_frontend/include/test_utils.hrl").

-import(user_command, [get_reg_info/1, reg_info_refine/1, new_user_record/1]).

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
                        user,smtp,undefined,undefined,0,undefined,undefined},
    ?_assert(new_user_record(Input) == ExpectedOutput).
