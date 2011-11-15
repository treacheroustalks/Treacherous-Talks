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
%%% @doc smtp_core
%%%
%%% A simple example callback module for `gen_smtp_server_session' that also serves as
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

-include_lib("datatypes/include/push_receiver.hrl").% #user{}
-record(state,
    {
        options = [] :: list(),
        hostname
    }).



%%==========intermodule interface===============
%%-----------------------------------------------------------------------------------------------
%% @doc init/4
%%
%% Initialize the callback module's state for a new session.
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
%% @end
%%------------------------------------------------------------------------------------------------
init(Hostname, SessionCount, Address, Options) ->
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
%% @doc handle_HELO/2 @end
%%
%% Handle the HELO verb from the client. Arguments are the Hostname sent by the client as
%% part of the HELO and the callback State.
%%
%% Return values are `{ok, State}' to simply continue with a new state, `{ok, MessageSize, State}'
%% to continue with the SMTP session but to impose a maximum message size (which you can determine
%% , for example, by looking at the IP address passed in to the init function) and the new callback
%% state. You can reject the HELO by returning `{error, Message, State}' and the Message will be
%% sent back to the client. The reject message MUST contain the SMTP status code, eg. 554.
%% @end
%%------------------------------------------------------------------------------------------------
handle_HELO(<<"invalid">>, State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    io:format("HELO from ~s~n", [Hostname]),
    {ok, 655360, State}. % 640kb of HELO should be enough for anyone.
    %If {ok, State} was returned here, we'd use the default 10mb limit


%%------------------------------------------------------------------------------------------------
%% @doc handle_EHLO/3 @end
%%
%% Handle the EHLO verb from the client. As with EHLO the hostname is provided as an argument,
%% but in addition to that the list of ESMTP Extensions enabled in the session is passed. This list
%% of extensions can be modified by the callback module to add/remove extensions.
%%
%% The return values are `{ok, Extensions, State}' where Extensions is the new list of extensions
%% to use for this session or `{error, Message, State}' where Message is the reject message as
%% with handle_HELO.
%% @end
%%------------------------------------------------------------------------------------------------
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
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
%% @doc handle_MAIL/2 @end
%%
%% Handle the MAIL FROM verb. The From argument is the email address specified by the
%% MAIL FROM command. Extensions to the MAIL verb are handled by the `handle_MAIL_extension'
%% function.
%%
%% Return values are either `{ok, State}' or `{error, Message, State}' as before.
%% @end
%%------------------------------------------------------------------------------------------------
handle_MAIL(<<"badguy@blacklist.com">>, State) ->
    {error, "552 go away", State};
handle_MAIL(From, State) ->
    io:format("Mail from ~s~n", [From]),
    % you can accept or reject the FROM address here
    {ok, State}.


%%------------------------------------------------------------------------------------------------
%% @doc handle_MAIL_extension/3 @end
%%
%% Handle an extension to the MAIL verb. Return either `{ok, State}' or `error' to reject
%% the option.
%% @end
%%------------------------------------------------------------------------------------------------
handle_MAIL_extension(<<"X-SomeExtension">> = Extension, State) ->
    io:format("Mail from extension ~s~n", [Extension]),
    % any MAIL extensions can be handled here
    {ok, State};
handle_MAIL_extension(Extension, _State) ->
    io:format("Unknown MAIL FROM extension ~s~n", [Extension]),
    error.


%%------------------------------------------------------------------------------------------------
%% @doc handle_RCPT/2 @end
%%
%% Handle RCPT verb to identify recipients
%% @end
%%------------------------------------------------------------------------------------------------
handle_RCPT(<<"nobody@example.com">>, State) ->
    {error, "550 No such recipient", State};
handle_RCPT(To, State) ->
    io:format("Mail to ~s~n", [To]),
    % you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.


%%------------------------------------------------------------------------------------------------
%% @doc handle_RCPT_extension/2 @end
%%
%% Handle an extension to the RCPT verb
%% @end
%%------------------------------------------------------------------------------------------------
handle_RCPT_extension(<<"X-SomeExtension">> = Extension, State) ->
    % any RCPT TO extensions can be handled here
    io:format("Mail to extension ~s~n", [Extension]),
    {ok, State};
handle_RCPT_extension(Extension, _State) ->
    io:format("Unknown RCPT TO extension ~s~n", [Extension]),
    error.


%%------------------------------------------------------------------------------------------------
%% @doc handle_DATA/4 @end
%%
%% Handle email message here
%% @end
%%------------------------------------------------------------------------------------------------
handle_DATA(_From, _To, <<>>, State) ->
    {error, "552 Message too small", State};
handle_DATA(From, To, Data, State) ->
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
%% @doc handle_RSET/1 @end
%%
%% Handle RSET verb.
%%------------------------------------------------------------------------------------------------
handle_RSET(State) ->
    % Reset any relevant internal state
    State.


%%------------------------------------------------------------------------------------------------
%% @doc handle_VRFY/2 @end
%%
%% Handle VRFY verb.
%%------------------------------------------------------------------------------------------------
handle_VRFY(<<"someuser">>, State) ->
    {ok, "someuser@" ++ smtp_util:guess_FQDN(), State};
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.


%%------------------------------------------------------------------------------------------------
%% @doc handle_other/3 @end
%%
%% Handle unknown SMTP verb.
%% @end
%%------------------------------------------------------------------------------------------------
handle_other(Verb, _Args, State) ->
    % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.


%%------------------------------------------------------------------------------------------------
%% @doc handle_AUTH/4 @end
%%
%% Handle AUTH verb.
%%
%% This callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
%% @end
%%------------------------------------------------------------------------------------------------
handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State)
  when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
        Digest ->
            {ok, State};
        _ ->
            error
    end;
handle_AUTH(_Type, _Username, _Password, _State) ->
    error.


%%------------------------------------------------------------------------------------------------
%% @doc code_change/3 @end
%%
%% Code update routine.
%% @end
%%------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------------------------
%% @doc terminate/2 @end
%%
%% Server termination routine.
%% @end
%%------------------------------------------------------------------------------------------------
terminate(Reason, State) ->
    {ok, Reason, State}.



%========Internal Functions========
%%------------------------------------------------------------------------------------------------
%% @doc relay/3 @end
%%
%% Relay a received email to a list of other email servers
%% @end
%%------------------------------------------------------------------------------------------------
relay(_From, [], _Data) ->
    ok;
relay(From, [To|Rest], Data) ->
    % relay message to email address
    [_User, Host] = string:tokens(To, "@"),
    io:format("Relay!~n"),
    gen_smtp_client:send({From, [To], erlang:binary_to_list(Data)}, [{relay, Host},{port,25}]),
    relay(From, Rest, Data).


%%------------------------------------------------------------------------------------------------
%% @doc simple_relay/4 @end
%%
%% Relay a received email to a single recipent
%% @end
%%------------------------------------------------------------------------------------------------
simple_relay(BinFrom, [BinTo|_Rest], BinData, MyHost) ->
    From = binary_to_list(BinFrom),
    To   = binary_to_list(BinTo),

    [_, ToHost] = string:tokens(To, "@"),

    io:format("Server got mail:~n"
              "From: ~p~n"
              "To: ~p~n"
              "Data: ~p~n"
              "MyHost: ~p~n",
              [BinFrom, BinTo, BinData, MyHost]),

    case ToHost of
        MyHost -> % when a mail reach its destination
            ParsedCmd = case command_parser:parse(BinData, mail) of
                            {login, {ok, User}} ->
                                PushReceiver = #push_receiver{
% @todo                                 pid = self(),
% @todo                                 args = [To, From]
                                 },
                                {login, {ok, {User, PushReceiver}}};
                            Other ->
                                Other
                        end,
            controller:handle_action(ParsedCmd, {fun smtp_output:reply/3,
                                                 [From, To]});
        _ -> %  when this game server is misused as a relay server
            {ok, relay_denied}
    end.


%%------------------------------------------------------------------------------------------------
%% @doc forward_mail/4 @end
%%
%% Use gen_smtp_client to send email
%% @end
%%------------------------------------------------------------------------------------------------
forward_mail(From, To, Data, ToHost) ->
    gen_smtp_client:send({From, [To], Data}, [{relay, ToHost}, {port,25}]),
    ok.
