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
%%% @author A.Rahim K.Mohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc ejabberd component that parses chat messages to
%%%  service@tt.hostname and handles them using our controller.
%%%
%%% @see command_parser
%%%
%%% @since : 19 Oct 2011 by Bermuda Triangle
%%%==================================================================
-module(tt_bot).
-behavior(gen_server).
-behavior(gen_mod).

-export([start_link/2]).

-export([start/2,
         stop/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% function registered to be called when ejabberd routes messages
% to this component.
-export([route/3]).

-include("ejabberd/include/ejabberd.hrl").
-include("ejabberd/include/jlib.hrl").
-include_lib("datatypes/include/user.hrl").

-define(PROCNAME, tt_bot).
-define(SUBDOMAIN, tt).
-define(SERVICE_USER, "service").

%%-------------------------------------------------------------------
%% @doc
%%  start link when it called by gen_server
%%-------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%%===================================================================
%% @ doc
%%  gen_mod callback functions
%%===================================================================

%%-------------------------------------------------------------------
%% @doc
%%  start function that called by ejabberd when start this component
%%  it initialize a child. To connect ejabberd and our backend we do ping
%%  here. So we need to start backend before ejabberd.
%%  @end
%%-------------------------------------------------------------------
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    pong = net_adm:ping('backend@127.0.0.1'),
    ChildSpec = {Proc,
                 {?MODULE, start_link, [Host, Opts]},
                 temporary,
                 1000,
                 worker,
                 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

%%-------------------------------------------------------------------
%% @doc
%%  this function is called when ever ejabberd want to stop this component
%%-------------------------------------------------------------------
stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%===================================================================
%% @ doc
%%  gen_server callback functions
%%===================================================================

%%-------------------------------------------------------------------
%% @doc
%%  Register a virtual host and tells ejabberd to route all messages
%%  to this virtual host to this module.
%%-------------------------------------------------------------------
init([Host, Opts]) ->
    % add a new virtual host / subdomain e.g. tt.localhost
    MyHost = gen_mod:get_opt_host(Host, Opts,
                                  atom_to_list(?SUBDOMAIN) ++ ".@HOST@"),
    ejabberd_router:register_route(MyHost, {apply, ?MODULE, route}),
    {ok, Host}.

%%-------------------------------------------------------------------
%% @doc
%%  handle call messages
%%-------------------------------------------------------------------
handle_call(stop, _From, Host) ->
    {stop, normal, ok, Host}.

%%-------------------------------------------------------------------
%% @doc
%%  handle cast messags
%%-------------------------------------------------------------------
handle_cast(_Msg, Host) ->
    {noreply, Host}.

%%-------------------------------------------------------------------
%% @doc
%%  handle all none call/cast messages
%%-------------------------------------------------------------------
handle_info(_Msg, Host) ->
    {noreply, Host}.

%%-------------------------------------------------------------------
%% @doc
%%  This function is called by a gen_server when it is about to
%%  terminate. It should be the opposite of Module:init/1 and do any necessary
%%  cleaning up. When it returns, the gen_server terminates with Reason.
%%  The return value is ignored
%%-------------------------------------------------------------------
terminate(_Reason, Host) ->
    ejabberd_router:unregister_route(Host),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, Host, _Extra) ->
    {ok, Host}.


%%-------------------------------------------------------------------
%% @ doc
%%  This function will get all messages that come to @tt.hostname.
%%
%%  Only messages to ?SERVICE_USER@tt.hostname are processed, the
%%  rest are logged and ignored.
%%
%%  * 'presence' messages are echoed back to the relevant user.
%%  * 'chat' messages that contain a body (text actually sent by
%%    a client) are parsed, and handld by our controller.
%%    Other chat messages, e.g. indicating whe someone starts writing
%%    but before they've sent the message, are ignored.
%%
%%  This function also handle the presence and subscription messages
%%-------------------------------------------------------------------
route(From,
      To = #jid{user=?SERVICE_USER},
      {xmlelement, "presence", _, _} = Packet) ->
    case xml:get_tag_attr_s("type", Packet) of
        "subscribe" ->
            send_presence(To, From, "subscribe");
        "subscribed" ->
            send_presence(To, From, "subscribed"),
            send_presence(To, From, "");
        "unsubscribe" ->
            send_presence(To, From, "unsubscribed"),
            send_presence(To, From, "unsubscribe");
        "unsubscribed" ->
            send_presence(To, From, "unsubscribed");
        "" ->
            send_presence(To, From, "");
        "unavailable" ->
            ok;
        "probe" ->
            send_presence(To, From, "");
        _Other ->
            ?INFO_MSG("Other kind of presence~n~p", [Packet])
    end,
    ok;

route(From,
      To = #jid{user=?SERVICE_USER},
      Packet = {xmlelement, "message", _, _}) ->
    case get_chat_body(Packet) of
        {error, Reason} ->
            ?INFO_MSG("Ignoring message because ~p. Packet:~n~p~n",
                      [Reason, Packet]);
        Body ->
            ParsedCmd = command_parser:parse(Body, im),
            ?INFO_MSG("Body ~p parsed as command ~p.~n",
                      [Body, ParsedCmd]),
            Callback = {fun tt_xmpp_output:reply/3,[To,From]},
            controller:handle_action(ParsedCmd, Callback)
    end,
    ok;

route(From, To, Packet) ->
    ?INFO_MSG("Component: ~p~n"
              "Ignoring message: ~p~n"
              "From: ~p~n"
              "To: ~p.~n",
              [?MODULE, From, To, Packet]),
    ok.


%%===================================================================
%% @ doc
%%  internal functions
%%===================================================================

get_chat_body(Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    BodyTag = xml:get_subtag(Packet, "body"),
    case Type of
        "chat" when BodyTag /= false ->
            BodyStr = xml:get_tag_cdata(BodyTag),
            BodyBin = list_to_binary(BodyStr),
            BodyBin;
        _ ->
            {error, "no body in this message."}
    end.


%get_ip(From) ->
%    Info = ejabberd_sm:get_user_info(From#jid.user,
%                                     From#jid.server,
%                                     From#jid.resource),
%    {ip, {Ip, _Port}} = lists:keyfind(ip, 1, Info),
%    Ip.


%%-------------------------------------------------------------------
%% @doc
%%  send presence
%%-------------------------------------------------------------------
send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To,
                          {xmlelement, "presence", [{"type", TypeStr}], []}).
