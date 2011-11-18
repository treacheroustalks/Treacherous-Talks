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
%%% @author A.Rahim Kadkhodamohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc xmpp_client to check ejabberd by using exmpp client
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(xmpp_client).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include_lib("eunit/include/eunit.hrl").

% API
-export([start_link/2, stop/1]).

-export([login/3, xmpp_call/3, xmpp_cast/3,
         last_received_msg/1, clear_last_received/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {session, msg, my_jid, last_received_msg, reply_expected,
                reply_to = undefined}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  use to start genserver to start process to make session
%%  and connect to ejabberd. We can provide setup parameter to setup
%%  session. 
%% 
%% @spec start_link(Sender::string(), Password::string()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @end
%%-------------------------------------------------------------------
start_link(Sender, Password) when is_atom(Sender) ->
    gen_server:start_link({local, Sender}, ?MODULE,
        [atom_to_list(Sender) ++ "@localhost", Password], []).

%%-------------------------------------------------------------------
%% @doc
%%  send a message to the recipient and return first xmpp chat message
%% that is received after the chat.
%%-------------------------------------------------------------------
xmpp_call(Sender, To, SendMsg) ->
    gen_server:call(Sender, {xmpp_call, To, SendMsg}).


%%-------------------------------------------------------------------
%% @doc
%%  send a message to the recipient in cast fashion.
%%-------------------------------------------------------------------
xmpp_cast(Sender, To, SendMsg) ->
    gen_server:cast(Sender, {xmpp_call, To, SendMsg}).

%%-------------------------------------------------------------------
%% @doc
%%  stop gen_server and exmpp application
%%-------------------------------------------------------------------
stop(Sender) ->
    gen_server:call(Sender, stop).

%%-------------------------------------------------------------------
%% @doc
%%  return the last received message
%%-------------------------------------------------------------------
last_received_msg(Sender) ->
    gen_server:call(Sender, last_received_msg).

clear_last_received(Sender) ->
    gen_server:call(Sender, clear_last_received).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%-------------------------------------------------------------------
%% @doc
%%  make session and login to the ejabberd server
%% @end
%%-------------------------------------------------------------------
init([Sender, Password]) ->
    {Session, JID} = login(exmpp, Sender, Password),
    {ok, #state{session = Session, my_jid = JID}}.

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
    {MySession, MyJID}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From , State = #state{session=MySession}) ->
    exmpp_session:stop(MySession),
    {stop, normal, ok, State};

handle_call({xmpp_call, To, SendMsg}, From, State) ->
    send_chat(To, State#state.my_jid, State#state.session, SendMsg),
    {noreply, State#state{msg = SendMsg, reply_to=From}};

handle_call(last_received_msg, _From, State) ->
    {reply, State#state.last_received_msg, State};

handle_call(clear_last_received, _From, State) ->
    NewState = State#state{last_received_msg = undefined},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = no_support,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc
%%  handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({xmpp_cast, To, SendMsg}, State) ->
    send_chat(To, State#state.my_jid, State#state.session, SendMsg),
    {no_reply, State#state{msg = SendMsg, reply_to=undefined}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages. Any message that send to the
%% process will process by this function.  If it is a stop mesage it will
%% stop the session. Otherwise it is a message that will process
%% @end
%%--------------------------------------------------------------------
handle_info(stop, State= #state{session=MySession}) ->
    exmpp_session:stop(MySession),
    {stop, normal, State};

%% This is how this module, as an exmpp client, receives messages sent
%% to this jid by ejabberd. If a response is expected by a call,
%% the message is sent as a reply to that call. Otherwise, the message
%% is just stored in the state.
handle_info(#received_packet{packet_type=message,
                             raw_packet=Packet,
                             type_attr="chat"},
            State=#state{reply_to=undefined}) ->
    Body = raw_packet_body_string(Packet),
    {noreply, State#state{last_received_msg = Body}};

% #state.reply_to being set implies a call has not been
% replied to.
handle_info(#received_packet{packet_type=message,
                             raw_packet=Packet,
                             type_attr="chat"},
            State=#state{reply_to=From}) ->
    Body = raw_packet_body_string(Packet),
    gen_server:reply(From, Body),
    NewState = State#state{last_received_msg = undefined,
                           reply_to = undefined},
    {noreply, NewState};

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
%%  registerd, it will register the user first.
%% @end
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
                                exmpp_presence:available(), "Test Client Ready")),
   {ok, MySession}.


%%-------------------------------------------------------------------
%% To is a string ejabberd id
%% From is a #jid from exmpp_jid
%% Message is a string
%%-------------------------------------------------------------------
send_chat(To, From, Session, Message) ->
    TmpPacket1 = exmpp_message:chat(Message),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket1, <<"from">>, From#jid.raw),
    TmpPacket3 = exmpp_xml:set_attribute(TmpPacket2, <<"to">>, To),
    ReadyPacket = exmpp_xml:remove_attribute(TmpPacket3, <<"id">>),
    exmpp_session:send_packet(Session, ReadyPacket),
    ok.


%%-------------------------------------------------------------------
%% Returns the string Body contends of an exmpp Packet.
%%-------------------------------------------------------------------
raw_packet_body_string(Packet) ->
    binary_to_list(exmpp_message:get_body(Packet)).



    % when messages go wrong you can use this function to extract info
%    _PrintMsg = lists:flatten(
%                  io_lib:format("Not handeled Message ~p from ~p to ~p~n",
%                                [Body, exmpp_stanza:get_sender(Packet),
%                                 exmpp_stanza:get_recipient(Packet)])),
