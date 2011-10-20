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

% EUnit auto test------------------------------------------------------
smtp_core_test_() ->
    app_start(),

    BinStr = ?SAMPLE_REGISTER,
    ExpectedUserInfo = #user{nick = "Lin", email = "ss@pcs", password = "QWER",
                             name = "Agner Erlang"},
    Output = command_parser:parse(BinStr),
    app_stop(some_client),
    ?_test(?assertEqual({register, {ok, ExpectedUserInfo}}, Output)).

simple_relay_test_() ->
    app_start(),

    User  = <<"user@user.pcs">>,
    User2 = <<"user2@user.pcs">>,
    Game  = <<"game@game.pcs">>,
    RegCmdMail = ?SAMPLE_REGISTER,
    UpdCmdMail = ?SAMPLE_UPDATE,
    LoginCmdMail = ?SAMPLE_LOGIN,
    NonCmdMail = <<"hello">>,
    UserHost = "user.pcs",
    GameHost= "game.pcs",

    % create mockers
    meck:new(gen_smtp_client), % mock gen_smtp_client
    meck:expect(gen_smtp_client, send, fun({_, [_], _}, [_|_]) -> {ok, self()} end),
    meck:new(controller),% mock controller
    meck:expect(controller, create_user, fun(_User) -> #user{} end),
    meck:expect(controller, update_user, fun(_User) -> #user{name = "lin"} end),
    meck:expect(controller, login_user, fun(_User) -> #user{name = "andre"} end),

    % Use SMTP server on user side-----------------------------------------
    UsersInTheSameDomain = simple_relay(User, [User2], NonCmdMail, UserHost),

    % Use SMTP server as game frontend-------------------------------------
    ReceivingNonCmdMail = simple_relay(User, [Game], NonCmdMail, GameHost),
    ReceivingRegCmdMail = simple_relay(User, [Game], RegCmdMail, GameHost),
    ReceivingUpdCmdMail = simple_relay(User, [Game], UpdCmdMail, GameHost),
    ReceivingLoginCmdMail = simple_relay(User, [Game], LoginCmdMail, GameHost),

    % clean mockers
    meck:unload(controller),
    meck:unload(gen_smtp_client),

    app_stop(some_client),
    [
      ?_test(?assertEqual(UsersInTheSameDomain,
                          {ok,{mail_stored,<<"hello">>}})),
      ?_test(?assertEqual(ReceivingNonCmdMail,
                          {ok, unknown_command})),
      ?_test(?assertEqual(ReceivingRegCmdMail,
                          {ok,{reg_request_sent, #user{} }})),
      ?_test(?assertEqual(ReceivingUpdCmdMail,
                          {ok,{reg_request_sent, #user{name = "lin"} }})),
      ?_test(?assertEqual(ReceivingLoginCmdMail,
                          ok))
    ].
