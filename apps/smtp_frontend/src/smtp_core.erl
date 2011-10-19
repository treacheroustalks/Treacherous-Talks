%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @module smtp_core @end
%%%
%%% @doc A simple example callback module for `gen_smtp_server_session' that also serves as
%%% documentation for the required callback API.
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(smtp_core).
-behaviour(gen_smtp_server_session).

%Export for gen_smtp_server_session callback
-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
    handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
    handle_other/3, handle_AUTH/4, code_change/3, terminate/2]).

-define(RELAY, true).
-define(ECHO_DISABLED, true). %set to false to enable echo_bot

%Export for eunit test
-export([simple_relay/4, forward_mail/4]).

-include_lib("datatypes/include/user.hrl").% -record(user,{})
-include("include/user_command.hrl").% -record(reg_info,{})

-record(state,
    {
        options = [] :: list(),
        hostname
    }).



%%==========intermodule interface===============
%%-----------------------------------------------------------------------------------------------
%% @function init/4 @end
%% @doc Initialize the callback module's state for a new session.
%% The arguments to the function are the SMTP server's hostname (for use in the SMTP anner),
%% The number of current sessions (eg. so you can do session limiting), the IP address of the
%% connecting client, and a freeform list of options for the module. The Options are extracted
%% from the `callbackoptions' parameter passed into the `gen_smtp_server_session' when it was
%% started.
%%
%% If you want to continue the session, return `{ok, Banner, State}' where Banner is the SMTP
%% banner to send to the client and State is the callback module's state. The State will be passed
%% to ALL subsequent calls to the callback module, so it can be used to keep track of the SMTP
%% session. You can also return `{stop, Reason, Message}' where the session will exit with Reason
%% and send Message to the client.
%%------------------------------------------------------------------------------------------------
init(
  Hostname,
  SessionCount,
  Address,
  Options) ->
    io:format("peer: ~p~n", [Address]),
    case SessionCount > 20 of
        false ->
            Banner = [Hostname, " ESMTP smtp_server_example"],
            State = #state{options = Options, hostname = Hostname},
            {ok, Banner, State};
        true ->
            io:format("Connection limit exceeded~n"),
            {stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}
    end.


%%------------------------------------------------------------------------------------------------
%% @function handle_HELO/2 @end
%%
%% @doc Handle the HELO verb from the client. Arguments are the Hostname sent by the client as
%% part of the HELO and the callback State.
%%
%% Return values are `{ok, State}' to simply continue with a new state, `{ok, MessageSize, State}'
%% to continue with the SMTP session but to impose a maximum message size (which you can determine
%% , for example, by looking at the IP address passed in to the init function) and the new callback
%% state. You can reject the HELO by returning `{error, Message, State}' and the Message will be
%% sent back to the client. The reject message MUST contain the SMTP status code, eg. 554.
%%------------------------------------------------------------------------------------------------
handle_HELO(
  <<"invalid">>,
  State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_HELO(
  <<"trusted_host">>,
  State) ->
    {ok, State}; %% no size limit because we trust them.
handle_HELO(
  Hostname,
  State) ->
    io:format("HELO from ~s~n", [Hostname]),
    {ok, 655360, State}. % 640kb of HELO should be enough for anyone.
    %If {ok, State} was returned here, we'd use the default 10mb limit


%%------------------------------------------------------------------------------------------------
%% @function handle_EHLO/3 @end
%%
%% @doc Handle the EHLO verb from the client. As with EHLO the hostname is provided as an argument,
%% but in addition to that the list of ESMTP Extensions enabled in the session is passed. This list
%% of extensions can be modified by the callback module to add/remove extensions.
%%
%% The return values are `{ok, Extensions, State}' where Extensions is the new list of extensions
%% to use for this session or `{error, Message, State}' where Message is the reject message as
%% with handle_HELO.
%%------------------------------------------------------------------------------------------------
handle_EHLO(
  <<"invalid">>,
  _Extensions,
  State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(
  Hostname,
  Extensions,
  State) ->
    io:format("EHLO from ~s~n", [Hostname]),
    % You can advertise additional extensions, or remove some defaults
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
        true ->
            % auth is enabled, so advertise it
            Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
        false ->
            Extensions
    end,
    {ok, MyExtensions, State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_MAIL/2 @end
%%
%% @doc Handle the MAIL FROM verb. The From argument is the email address specified by the
%% MAIL FROM command. Extensions to the MAIL verb are handled by the `handle_MAIL_extension'
%% function.
%%
%% Return values are either `{ok, State}' or `{error, Message, State}' as before.
%%------------------------------------------------------------------------------------------------
handle_MAIL(
  <<"badguy@blacklist.com">>,
  State) ->
    {error, "552 go away", State};
handle_MAIL(
  From,
  State) ->
    io:format("Mail from ~s~n", [From]),
    % you can accept or reject the FROM address here
    {ok, State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_MAIL_extension/3 @end
%%
%% @doc Handle an extension to the MAIL verb. Return either `{ok, State}' or `error' to reject
%% the option.
%%------------------------------------------------------------------------------------------------
handle_MAIL_extension(
  <<"X-SomeExtension">> = Extension,
  State) ->
    io:format("Mail from extension ~s~n", [Extension]),
    % any MAIL extensions can be handled here
    {ok, State};
handle_MAIL_extension(
  Extension,
  _State) ->
    io:format("Unknown MAIL FROM extension ~s~n", [Extension]),
    error.


%%------------------------------------------------------------------------------------------------
%% @function handle_RCPT/2 @end
%%
%% @doc Handle RCPT verb to identify recipients
%%------------------------------------------------------------------------------------------------
handle_RCPT(
  <<"nobody@example.com">>,
  State) ->
    {error, "550 No such recipient", State};
handle_RCPT(
  To,
  State) ->
    io:format("Mail to ~s~n", [To]),
    % you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_RCPT_extension/2 @end
%%
%% @doc Handle an extension to the RCPT verb
%%------------------------------------------------------------------------------------------------
handle_RCPT_extension(
  <<"X-SomeExtension">> = Extension,
  State) ->
    % any RCPT TO extensions can be handled here
    io:format("Mail to extension ~s~n", [Extension]),
    {ok, State};
handle_RCPT_extension(
  Extension,
  _State) ->
    io:format("Unknown RCPT TO extension ~s~n", [Extension]),
    error.


%%------------------------------------------------------------------------------------------------
%% @function handle_DATA/4 @end
%%
%% @doc Handle email message here
%%------------------------------------------------------------------------------------------------
handle_DATA(
  _From,
  _To,
  <<>>,
  State) ->
    {error, "552 Message too small", State};
handle_DATA(
  From,
  To,
  Data,
  State) ->
    % some kind of unique id
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= 
                               erlang:md5(term_to_binary(erlang:now()))]),
    % if RELAY is true, then relay email to email address, else send email data to console
    case proplists:get_value(relay, State#state.options, false) of
        true -> relay(From, To, Data);
        false ->
            simple_relay(From, To, Data, State#state.hostname), %for echo test
            ok
    end,
    % At this point, if we return ok, we've accepted responsibility for the email
    {ok, Reference, State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_RSET/1 @end
%%
%% @doc Handle RSET verb.
%%------------------------------------------------------------------------------------------------
handle_RSET(
  State) ->
    % Reset any relevant internal state
    State.


%%------------------------------------------------------------------------------------------------
%% @function handle_VRFY/2 @end
%%
%% @doc Handle VRFY verb.
%%------------------------------------------------------------------------------------------------
handle_VRFY(
  <<"someuser">>,
  State) ->
    {ok, "someuser@" ++ smtp_util:guess_FQDN(), State};
handle_VRFY(
  _Address,
  State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_other/3 @end
%%
%% @doc Handle unknown SMTP verb.
%%------------------------------------------------------------------------------------------------
handle_other(
  Verb,
  _Args,
  State) ->
    % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.


%%------------------------------------------------------------------------------------------------
%% @function handle_AUTH/4 @end
%%
%% @doc Handle AUTH verb.
%%
%% @note this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
%%------------------------------------------------------------------------------------------------
handle_AUTH(
  Type,
  <<"username">>,
  <<"PaSSw0rd">>,
  State)
  when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH(
  'cram-md5',
  <<"username">>,
  {Digest, Seed},
  State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
        Digest ->
            {ok, State};
        _ ->
            error
    end;
handle_AUTH(
  _Type,
  _Username,
  _Password,
  _State) ->
    error.

%%------------------------------------------------------------------------------------------------
%% @function code_change/3 @end
%%
%% @doc Code update routine.
%%------------------------------------------------------------------------------------------------
code_change(
  _OldVsn,
  State,
  _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------
%% @function terminate/2 @end
%%
%% @doc Server termination routine.
%%------------------------------------------------------------------------------------------------
terminate(
  Reason,
  State) ->
    {ok, Reason, State}.



%========Internal Functions========
%%------------------------------------------------------------------------------------------------
%% @function relay/3 @end
%%
%% @doc Relay a received email to a list of other email servers
%%------------------------------------------------------------------------------------------------
relay(
  _From,
  [],
  _Data) ->
    ok;
relay(
  From,
  [To|Rest],
  Data) ->
    % relay message to email address
    [_User, Host] = string:tokens(To, "@"),
    io:format("Relay!~n"),
    gen_smtp_client:send({From, [To], erlang:binary_to_list(Data)}, [{relay, Host},{port,25}]),
    relay(From, Rest, Data).

%%------------------------------------------------------------------------------------------------
%% @function simple_relay/4 @end
%%
%% @doc Relay a received email to a single recipent
%%------------------------------------------------------------------------------------------------
simple_relay(
  BinFrom,
  [BinTo|_Rest],
  BinData,
  MyHost) ->
    From = binary_to_list(BinFrom),
    To   = binary_to_list(BinTo),
    Data = binary_to_list(BinData),

    [_, ToHost] = string:tokens(To, "@"),
    [_, FromHost] = string:tokens(From, "@"),

    case ToHost of
        FromHost
          when FromHost == MyHost -> % when sender and receipent are on our server
            {ok, {mail_stored, BinData}};
%        MyHost % when a mail arrives its destination
%          when false /= ?ECHO_DISABLED -> % when echo mode on
%            io:format("###Echo back ~s to ~s~n", [To, From]),
%            forward_mail(To, From, Data, FromHost),
%            io:format("###Echo ~s to ~s sent~n", [MyHost, FromHost]),
%            {ok, {echo, {To, From}}};
        MyHost -> % when echo mode off
            io:format("message from ~s to ~p ~n~n~s~n", [From, To, Data]),
            % parse registration command from email body
            RegInfoRecord = user_command:get_reg_info(BinData),
            RegReport = case RegInfoRecord of
                {ok, RegInfo} -> % if there is register command
                    % convert #reg_info{name, password...} to #user{name, password...}
                    UserInfo = user_command:new_user_record(RegInfo),
                    % call controller API to register user at backend
                    UserRec = controller:create_user(undefined, UserInfo),
                    {ok, {reg_request_sent, UserRec}};
                Other -> % if there is no register command or invalid register command
                    Other
            end,
            {ok, {cmd_parsed, [RegReport]}};
        _ -> % this isn't the mail's destination, forward it to its recipent
            io:format("###Forwarding ~s to ~s...~n", [From, To]),
            forward_mail(From, To, Data, ToHost),
            io:format("###Forward ~s to ~s sent!~n", [MyHost, ToHost]),
            {ok, {forward, {From, To}}}
    end.

%%------------------------------------------------------------------------------------------------
%% @function forward_mail/4 @end
%%
%% @doc Use gen_smtp_client to send email
%%------------------------------------------------------------------------------------------------
forward_mail(
  From,
  To,
  Data,
  ToHost) ->
    gen_smtp_client:send({From, [To], Data}, [{relay, ToHost}, {port,25}]).
