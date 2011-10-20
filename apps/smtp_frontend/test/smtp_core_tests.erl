-module(smtp_core_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include("include/user_command.hrl").% -record(reg_info,{})
-include("include/test_utils.hrl"). % ?SAMPLE_EMAIL

-import(smtp_core, [simple_relay/4, forward_mail/4]).

% EUnit auto test------------------------------------------------------
smtp_core_test_()->
    BinStr = ?SAMPLE_EMAIL,
    ExpectedRegRec = {reg_info,undefined,undefined,undefined,undefined},
    ExpectedUserRec = {user,undefined,undefined,undefined,undefined,undefined,user,
                       undefined,undefined,undefined,0,undefined,undefined},
    ExpectedRegInfo = {reg_info,<<"Lin">>,<<"QWER">>,<<"ss@pcs">>,<<"Agner Erlang">>},
    ExpectedUserInfo = {user,undefined,<<"Lin">>,<<"ss@pcs">>,<<"QWER">>,<<"Agner Erlang">>,
                        user,smtp,undefined,undefined,0,undefined,undefined},
    UserRec = #user{},
    RegRec = #reg_info{},
    {ok,RegInfo} = user_command:get_reg_info(BinStr),
    UserInfo = user_command:new_user_record(RegInfo),
    [
      ?_assert(UserRec==ExpectedUserRec),
      ?_assert(RegRec==ExpectedRegRec),
      ?_assert(RegInfo==ExpectedRegInfo),
      ?_assert(UserInfo==ExpectedUserInfo)
    ].

simple_relay_test_()->
    application:start(datatypes),

    User  = <<"user@user.pcs">>,
    User2 = <<"user2@user.pcs">>,
    Game  = <<"game@game.pcs">>,
    RegCmdMail = ?SAMPLE_EMAIL,
    NonCmdMail = <<"hello">>,
    UserHost = "user.pcs",
    GameHost= "game.pcs",

    % create mockers
    meck:new(gen_smtp_client), % mock gen_smtp_client
    meck:expect(gen_smtp_client, send, fun({_, [_], _}, [_|_]) -> {ok, self()} end),
    meck:new(controller),% mock controller
    meck:expect(controller, create_user, fun(_Id, _User) -> #user{} end),

    % Use SMTP server on user side-----------------------------------------
    ForwardUserMail = simple_relay(User, [Game], NonCmdMail, UserHost),
    UsersInTheSameDomain = simple_relay(User, [User2], NonCmdMail, UserHost),

    % Use SMTP server as game frontend-------------------------------------
    ReceivingNonCmdMail = simple_relay(User, [Game], NonCmdMail, GameHost),
    ReceivingRegCmdMail = simple_relay(User, [Game], RegCmdMail, GameHost),

    % clean mockers
    meck:unload(controller),
    meck:unload(gen_smtp_client),
    application:stop(datatypes),
    [
      ?_assert(ForwardUserMail == {ok,{forward,{"user@user.pcs","game@game.pcs"}}}),
      ?_assert(UsersInTheSameDomain == {ok,{mail_stored,<<"hello">>}}),
      ?_assert(ReceivingNonCmdMail == {ok,{cmd_parsed,[{error,no_reg_start}]}}),
      ?_assert(ReceivingRegCmdMail == {ok,{cmd_parsed,[{ok,{reg_request_sent, #user{}}}]}})
    ].
