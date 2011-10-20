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
-export([start_link/0,start_link/2,send_message/2, stop/0, last_received_msg/0]).

% gen_server callbacks
-export([init/1, init/2, login/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {session, msg, my_jid, last_received_msg}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  use to start genserver to start process to make session
%%  and connect to ejabberd. We can provide setup parameter to setup
%%  session.
%%  start_link() -> {ok,Pid} | ignore | {error,Error}
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,[], []).

%%-------------------------------------------------------------------
%% @ doc
%%  sart a session with the passed user and passowrd
%%-------------------------------------------------------------------
start_link(Sender, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
        [Sender, Password], []).

%%-------------------------------------------------------------------
%% @doc
%%  send a message to the recipient
%%-------------------------------------------------------------------
send_message(To, SendMsg) ->
    gen_server:call(?MODULE, {send_message, To, SendMsg}).

%%-------------------------------------------------------------------
%% @doc
%%  stop gen_server and exmpp application
%%-------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%-------------------------------------------------------------------
%% @doc
%%  return the last received message
%%-------------------------------------------------------------------
last_received_msg() ->
    gen_server:call(?MODULE, last_received_msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%  it sets the initial the paramters for login and session @end
%%--------------------------------------------------------------------
init([])->
    Sender = "echo@localhost",
    Password = "password",
    %SendMsg = "Hello World !",
    login(exmpp, Sender, Password).

%%-------------------------------------------------------------------
%% @doc
%%  make session and login to the ejabberd server
%% @end
%%-------------------------------------------------------------------
init(Sender, Password) ->
    login(exmpp, Sender, Password).

%%--------------------------------------------------------------------
%% @doc
%%
%% it starts exmpp application and starts a session with ejabberd
%% then it will return the state
%% @end
%%--------------------------------------------------------------------

login(App, Sender, Password) ->
    application:start(App),
    % Start XMPP session: Needed to start service (Like
    % exmpp_stringprep):
    MySession = exmpp_session:start(),
    % Create XMPP ID (Session Key):
    [User, Server] = string:tokens(Sender, "@"),
    MyJID = exmpp_jid:make(User, Server, random),
    % Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    % Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, Server, 5222),
    {ok, MySession} = session(MySession, Password),
    {ok, #state{session=MySession, my_jid = MyJID}}.

%%--------------------------------------------------------------------
%% @function  handle_call/3  @end
%%
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From , State = #state{session=MySession}) ->
    exmpp_session:stop(MySession),
    %application:stop(exmpp),
    %application:stop(exmpp_stringprep),
    %application:stop(inet),
    {stop, normal, ok, State};
    %{stop, normal, State};
handle_call({send_message, To, SendMsg}, _From, State) ->
    M = exmpp_message:chat(SendMsg),
    M2 = exmpp_stanza:set_sender(M, To),
    M3 = exmpp_stanza:set_recipient(M2, State#state.my_jid),
    echo_packet(State#state.session, M3),
    {reply, ok, State#state{msg = SendMsg}};
handle_call(last_received_msg, _From, State) ->
    {reply, State#state.last_received_msg, State};
handle_call(_Request, _From, State) ->
    Reply = no_support,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc
%%  handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages. Any message that send to the
%% process will process by this function.  If it is a stop mesage it will
%% stop the session. Otherwise it is a message that will process
%% @end
%%--------------------------------------------------------------------
handle_info(stop,State= #state{session=MySession}) ->
    exmpp_session:stop(MySession),
    application:stop(exmpp),
    application:stop(exmpp_stringprep),
    application:stop(inet),
    {stop, normal, State};

handle_info(#received_packet{packet_type=message,raw_packet=Packet,
                             type_attr="chat"} , State) ->

    Body = binary_to_list(exmpp_message:get_body(Packet)),
    % when messages go wrong you can use this function to extract info
    _PrintMsg = lists:flatten(
                  io_lib:format("Not handeled Message ~p from ~p to ~p~n",
                                [Body, exmpp_stanza:get_sender(Packet),
                                 exmpp_stanza:get_recipient(Packet)])),
    {noreply, State#state{last_received_msg = Body}};

handle_info(_Record, State) ->
    %io:format("~p~n", [Record]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored. @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{session=_MySession}) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Convert process state when code is changed @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc session(MySession, Password) -> {ok, Mysession} |
%%                                      {timeout, any()}
%%
%%  Try to send presence to ejabberd server and if the user is not
%%  registerd, it will register the user first. Then it will send a mesage to
%%  sample user in echo component of ejabberd. @end
%%--------------------------------------------------------------------

% We are connected. We now log in (and try registering if authentication fails)
session(MySession, Password) ->
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

   {ok, MySession}.
%%---------------------------------------------------------------------
%% @doc echo_packet(MySession, Packet) -> ok | {error, Error}
%%
%%  Send the same packet back for each message received. It just
%% change the sender and receiver. @end
%%---------------------------------------------------------------------
echo_packet(MySession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(MySession, NewPacket),
    ok.

