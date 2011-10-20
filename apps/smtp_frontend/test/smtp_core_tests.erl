-module(smtp_core_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datatypes/include/user.hrl").% #user{}
-include_lib("datatypes/include/game.hrl").% #game{}

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
    User  = <<"user@user.pcs">>,
    User2 = <<"user2@user.pcs">>,
    Game  = <<"game@game.pcs">>,
    RegCmdMail = ?SAMPLE_REGISTER,
    UpdCmdMail = ?SAMPLE_UPDATE,
    LoginCmdMail = ?SAMPLE_LOGIN,
    CreateGameCmdMail = ?SAMPLE_CREATE,
    NonCmdMail = <<"hello">>,
    UserHost = "user.pcs",
    GameHost = "game.pcs",
    ExpectedGame = #game{
                       name = "awesome_game", press = "white",
                       order_phase = "4H", retreat_phase = "3H30M",
                       build_phase = "2H40M", waiting_time = "2D5H20M",
                       creator_id = undefined},

    app_start(),
    % create mockers
    meck:new(gen_smtp_client), % mock gen_smtp_client
    meck:expect(gen_smtp_client, send, fun({_, [_], _}, [_|_]) ->
                                           {ok, self()} end),
    meck:new(controller),% mock controller
    meck:expect(controller, create_user, fun(_User) -> #user{} end),
    meck:expect(controller, update_user, fun(_User) -> #user{name = "lin"} end),
    meck:expect(controller, get_user, fun(_Type, _Key) -> #user{id = 12345,
                                                                name = "lin"} end),
    meck:expect(controller, login_user, fun(_User) -> #user{name = "andre"} end),
    meck:expect(controller, new_game, fun(_Game) -> ExpectedGame end),

    % Use SMTP server on user side-----------------------------------------
    UsersInTheSameDomain = simple_relay(User, [User2], NonCmdMail, UserHost),

    % Use SMTP server as game frontend-------------------------------------
    ReceivingNonCmdMail = simple_relay(User, [Game], NonCmdMail, GameHost),
    ReceivingRegCmdMail = simple_relay(User, [Game], RegCmdMail, GameHost),
    ReceivingUpdCmdMail = simple_relay(User, [Game], UpdCmdMail, GameHost),
    ReceivingLoginCmdMail = simple_relay(User, [Game], LoginCmdMail, GameHost),
    ReceivingCreateGameCmdMail = simple_relay(User, [Game], CreateGameCmdMail,
                                              GameHost),

    % clean mockers
    meck:unload(controller),
    meck:unload(gen_smtp_client),

    app_stop(some_client),
    [
      ?_test(?assertEqual({ok,{mail_stored,<<"hello">>}},
                          UsersInTheSameDomain)),
      ?_test(?assertEqual({ok, unknown_command},
                          ReceivingNonCmdMail)),
      ?_test(?assertEqual({ok,{reg_request_sent, #user{}}},
                          ReceivingRegCmdMail)),
      ?_test(?assertEqual({ok,{reg_request_sent, #user{name = "lin"} }},
                          ReceivingUpdCmdMail)),
      ?_test(?assertEqual(ok,
                          ReceivingLoginCmdMail)),
      ?_test(?assertEqual({ok,{reg_request_sent, ExpectedGame }},
                          ReceivingCreateGameCmdMail))
    ].
