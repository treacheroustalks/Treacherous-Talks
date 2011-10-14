%%%-------------------------------------------------------------------
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%--------------------------------------------------------------------
%%% @module xmpp_client @end
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%% @doc xmpp_client to check ejabberd by using exmpp client
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(xmpp_client).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

% API
-export([start_link/0]).

% gen_server callbacks
-export([init/1,login/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {session, msg}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @function start_link/0 @end
%%
%% @doc start_link() -> {ok,Pid} | ignore | {error,Error}
%% use genserver to start a process to do the echo test
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,[], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @function init/1 @end
%%
%% @doc it gets the initial the paramters
%%--------------------------------------------------------------------
init([])->
    login(exmpp).

%%--------------------------------------------------------------------
%% @function init/4 @end
%%
%% @doc it starts exmpp application and starts a session with ejabberd
%% then it will return the state
%%--------------------------------------------------------------------

login(App) ->
    JID = "echo@localhost",
    Password = "password",
    Msg = "Hello World !",
    Recipient = "tester@echo.localhost",

    application:start(App),
    % Start XMPP session: Needed to start service (Like
    % exmpp_stringprep):
    MySession = exmpp_session:start(),
    % Create XMPP ID (Session Key):
    [User, Server] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, Server, random),
    % Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    % Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, Server, 5222),
    {ok, MySession} = session(MySession, MyJID, Password, Recipient, Msg),
    {ok, #state{session=MySession, msg=Msg}}.

%%--------------------------------------------------------------------
%% @function  handle_call/3  @end
%%
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = no_support,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @function: handle_cast/2 @end
%%
%% @doc handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @function: handle_info/2 @end
%%
%% @doc  Handling all non call/cast messages. Any message that send to the
%% process will process by this function.  If it is a stop mesage it will
%% stop the session. Otherwise it is a message that will process
%%--------------------------------------------------------------------
handle_info(stop,State= #state{session=MySession}) ->
    exmpp_session:stop(MySession),
    {stop, normal, State};
handle_info(Record = #received_packet{packet_type=message,raw_packet=Packet,
                                      type_attr=Type} ,#state{session=MySession,
                                           msg=Msg} = State)
  when Type =/= "error" ->
    case exmpp_message:get_body(Packet) =:= list_to_binary(Msg) of
        false ->
            io:format("~p~n", [Record]),
            echo_packet(MySession, Packet),
            {noreply, State};
        true ->
            io:format("Handshaking is done between ~p and ~p ~n",
            [exmpp_stanza:get_sender(Packet),
            exmpp_stanza:get_recipient(Packet)]),
            exmpp_session:stop(MySession),
            application:stop(exmpp_stringprep),
            application:stop(inet),
            {stop, normal, State}
    end;
handle_info(Record, State) ->
    io:format("~p~n", [Record]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @function terminate/2 @end
%%
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{session=_MySession}) ->
    ok.

%%--------------------------------------------------------------------
%% @function code_change/3 @end
%%
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @function: session(MySession, _MyJID, Password) -> {ok, Mysession} |
%%                                                   {timeout, any()}
%% @end
%%
%% @doc Try to send presence to ejabberd server and if the user is not
%%  registerd, it will register the user first. Then it will send a mesage to
%%  sample user in echo component of ejabberd.
%%--------------------------------------------------------------------

% We are connected. We now log in (and try registering if authentication fails)
session(MySession, MyJID, Password, Recipient, Msg) ->
    % Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
            % Try creating a new user:
            io:format("Register~n",[]),
            % In a real life client, we should trap error case here
            % and print the correct message.
            exmpp_session:register_account(MySession, Password),
            % After registration, retry to login:
            exmpp_session:login(MySession)
    end,
    % We explicitly send presence:
    exmpp_session:send_packet(MySession,
                               exmpp_presence:set_status(
                                exmpp_presence:available(), "Echo Ready")),

    % Prepare a message packet to send to user.
    M = exmpp_message:chat(Msg),
    M2 = exmpp_stanza:set_sender(M, Recipient),
    M3 = exmpp_stanza:set_recipient(M2, MyJID),
    echo_packet(MySession, M3),
    {ok, MySession}.

%%---------------------------------------------------------------------
%% @function echo_packet(MySession, Packet) -> ok | {error, Error}
%% @end
%%
%% @doc Send the same packet back for each message received. It just
%% change the sender and receiver.
%%---------------------------------------------------------------------
echo_packet(MySession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(MySession, NewPacket),
    ok.
