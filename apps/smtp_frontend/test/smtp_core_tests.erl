-module(smtp_core_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include_lib("command_parser/include/records.hrl").% -record(reg_info,{})
-include_lib("command_parser/include/test_utils.hrl"). % ?SAMPLE_ ..

-import(smtp_core, [simple_relay/4, forward_mail/4]).

%% startup
apps() ->
    [datatypes, command_parser].

app_start() ->
    [ ?assertEqual(ok, application:start(App)) || App <- apps()],
    error_logger:tty(false).

%% teardown
app_stop(_Client) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

smtp_core_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     fun smtp_core/0}.

simple_relay_test_() ->
    {setup,
     fun app_start/0,
     fun app_stop/1,
     fun simple_relay/0}.

% EUnit auto test------------------------------------------------------
smtp_core()->
    BinStr = ?SAMPLE_REGISTER,
    ExpectedUserInfo = {user,undefined,<<"Lin">>,<<"ss@pcs">>,<<"QWER">>,<<"Agner Erlang">>,
                        user,smtp,undefined,undefined,0,undefined,undefined},
    Output = command_parser:parse(BinStr),
    ?_test(?assertEqual({register, {ok, ExpectedUserInfo}}, Output)).

simple_relay()->
    User  = <<"user@user.pcs">>,
    User2 = <<"user2@user.pcs">>,
    Game  = <<"game@game.pcs">>,
    RegCmdMail = ?SAMPLE_REGISTER,
    NonCmdMail = <<"hello">>,
    UserHost = "user.pcs",
    GameHost= "game.pcs",

    % create mockers
    meck:new(gen_smtp_client), % mock gen_smtp_client
    meck:expect(gen_smtp_client, send, fun({_, [_], _}, [_|_]) -> {ok, self()} end),
    meck:new(controller),% mock controller
    meck:expect(controller, create_user, fun(_User) -> #user{} end),

    % Use SMTP server on user side-----------------------------------------
    UsersInTheSameDomain = simple_relay(User, [User2], NonCmdMail, UserHost),

    % Use SMTP server as game frontend-------------------------------------
    ReceivingNonCmdMail = simple_relay(User, [Game], NonCmdMail, GameHost),
    ReceivingRegCmdMail = simple_relay(User, [Game], RegCmdMail, GameHost),

    % clean mockers
    meck:unload(controller),
    meck:unload(gen_smtp_client),
    [
      ?_test(?assertEqual({ok,{mail_stored,<<"hello">>}},
                          UsersInTheSameDomain)),
      ?_test(?assertEqual({ok, unknown_command},
                          ReceivingNonCmdMail)),
      ?_test(?assertEqual({ok,{reg_request_sent, #user{} }},
                          ReceivingRegCmdMail))
    ].
