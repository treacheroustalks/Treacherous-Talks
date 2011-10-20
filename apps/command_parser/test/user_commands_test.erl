-module(user_commands_test).
-include_lib("eunit/include/eunit.hrl").

-include("test_utils.hrl").


get_reg_info_test_() ->
    Output = {ok,{reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>}},
    ?_assert(user_commands:get_reg_info(?SAMPLE_REGISTER) == Output).

reg_info_refine_test_() ->
    Raw = [<<"NICKNAME: Lin">>,<<"PASSWORD: QWER">>,<<>>,<<"EMAIL: ss@pcs">>,
           <<"asdfasdlfadf">>,<<>>,<<"FULLNAME: Agner Erlang">>],
    Output = {ok,{reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>}},
    ?_assert(user_commands:reg_info_refine(Raw) == Output).

new_user_record_test_() ->
    Input = {reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>},
    ExpectedOutput = {user,undefined,<<"Lin">>,<<"ss@pcs">>,<<"QWER">>,<<"Agner Erlang">>,
                        user,smtp,undefined,undefined,0,undefined,undefined},
    ?_assert(user_commands:new_user_record(Input) == ExpectedOutput).
