%%%===================================================================
%%% @copyright
%%% COPYRIGHT
%%% @end
%%%-------------------------------------------------------------------
%%% @author A.Rahim K.Mohammadi <r.k.mohammadi@gmail.com>
%%%
%%% @doc Register component added to ejabberd to get messages that send
%%%  to register@service.hostname and send them to our controller.
%%%
%%% @see command_parser
%%%
%%% @since : 19 Oct 2011 by Bermuda Triangle
%%%==================================================================
-module(tt_register_bot).
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

-export([route/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("datatypes/include/user.hrl").

-define(PROCNAME, tt_register_bot).
%% @doc the name for virtual domain
-define(BOTNAME, service).
-define(WRONG_MSG_STYLE, "Please send registration request as below it start
 with REGISTER and finish with END and enter keywords with capital letters").
-define(MSG_STYLE, "
REGISTER,
NICKNAME: your nick,
PASSWORD: pass,
FULLNAME: full name,
EMAIL: sth@sth
END").

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
%%  init function that gets a host name and register a virtual host and tells
%%  ejabberd to route all message that come to this virtual host to a specific
%%  funcation e.x here to route function in this module
%%-------------------------------------------------------------------
init([Host, Opts]) ->
    ?DEBUG("ECHO_BOT: Starting ~p", [atom_to_list(?BOTNAME)]),
    % add a new virtual host / subdomain "echo".hostname
    MyHost = gen_mod:get_opt_host(Host, Opts,
                                  atom_to_list(?BOTNAME) ++ ".@HOST@"),
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

%%===================================================================
%% @ doc
%%  internal functions
%%===================================================================

%%-------------------------------------------------------------------
%% @ doc
%%  this function will get all message that comes to @service.hostname
%%  if this message is not sent to register it will send an error responce
%%  back to the user. Otherwise, it will extract the registration information
%%   of the user and put it in a user record
%%  Then, it will send the record to backend to be registered
%%  This function also handle the presence and subscription messages
%%-------------------------------------------------------------------
route(From, To, {xmlelement, "presence", _, _} = Packet) ->
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
% handle registration message sent to "register" user at this virtual host
route(From, To = #jid{user="register"},
      {xmlelement, "message", _, _} = Packet) ->
    case xml:get_subtag_cdata(Packet, "body") of
        "" ->
            send_chat(To, From, strip_bom("Please do not send empty Message!")),
            ok;
        Body ->
            case xml:get_tag_attr_s("type", Packet) of
                "error" ->
                    ?ERROR_MSG("Received error message~n~p -> ~p~n~p",
                               [From, To, Packet]);
                "chat" ->
                    Info = ejabberd_sm:get_user_info(From#jid.user,
                                                     From#jid.server,
                                                     From#jid.resource),
                    {ip, {Ip, _Port}} = lists:keyfind(ip, 1, Info),
                    handle_command(From, To, Body, Ip);
                _ ->
                    send_chat(To, From, strip_bom(?WRONG_MSG_STYLE
                                             ++ "undifined message type"))
            end
    end,
    ok;
% handle all messages that send to a user except "rgister" at this virtual host
route(_From, To, {xmlelement, "message", _, _} = Packet) ->
    %[User, Server] = string:tokens(To, "@"),
    ?INFO_MSG("Sending message to someone else ~p:: ~n :::: ~n~p",
              [To#jid.user ,Packet]),
    ok;
route(From, To, Packet) ->
    ?INFO_MSG("not handled massage in ~p:~n from:~p~n to:~n~p Packet:~n~p",
              [?MODULE, From, To, Packet]),
    ok.

%%-------------------------------------------------------------------
%% @ doc
%%  strip the BOM or Byte Order Mark from the beginning of the body
%%-------------------------------------------------------------------
strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

%%-------------------------------------------------------------------
%% @doc
%%  send presence
%%-------------------------------------------------------------------
send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To,
                          {xmlelement, "presence", [{"type", TypeStr}], []}).

%%-------------------------------------------------------------------
%% @ doc
%%  send the message to a user as a chat message
%%-------------------------------------------------------------------
send_chat(From, To, Body) ->
    send_message(From, To, "chat", Body).

%%-------------------------------------------------------------------
%% @ doc
%%  prepare the xml chat mesage and route it to the user
%%-------------------------------------------------------------------
send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
               [{"type", TypeStr},
                {"from", jlib:jid_to_string(From)},
                {"to", jlib:jid_to_string(To)}],
               [{xmlelement, "body", [],
                 [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(From, To, XmlBody).

%%------------------------------------------------------------------
%% @doc
%%   get the sender, recipient, content and sender's Ip address
%%   parse the content and if it is valid it will make  a user record,
%%   send to controller to register the user.
%%   if the user's information is not correct it will send an
%%   error message to the user.
%%------------------------------------------------------------------
handle_command(From, To, Body, _Ip) ->
    case command_parser:parse(list_to_binary(Body)) of
        {register, {ok, UserInfo}} ->
            UserRec = controller:create_user(UserInfo),
            send_chat(To, From,
              io_lib:format("Registration result: ~p",
                    [UserRec]));
        {register, Error} ->
            io:format("wrong format from user ~p~n",[Error]),
            send_chat(To, From, strip_bom(?WRONG_MSG_STYLE ++ ?MSG_STYLE));
    {login, {ok, UserInfo}} ->
            case controller:login_user(UserInfo) of
                invalid ->
                    io:format("[login invalid] ~p", [UserInfo]),
                    send_chat(To, From,  strip_bom("Invalid login data"));
                Session ->
                    io:format("[login successful] session: ~p", [Session]),
                    send_chat(To, From,
                   strip_bom("Loggedn in, session id: "
                     ++ integer_to_list(Session)))
            end;
        {login, Error} ->
            send_chat(To, From,  strip_bom("login error")),
            io:format("[login error] ~p", [Error]);
    {update, {ok, ParsedUser}} ->
        [OldUser|_] = controller:get_user(#user.nick, ParsedUser#user.nick),
        NewUser = OldUser#user{password = ParsedUser#user.password,
                   name = ParsedUser#user.name},
        UserRec = controller:update_user(NewUser),
        send_chat(To, From,
               strip_bom(io_lib:format("update success: ~p",
                           [UserRec]))),
        io:format("[update success] ~p~n", [UserRec]);
        {update, Error} ->
            io:format("[update error] ~p", [Error]),
        send_chat(To, From,
               strip_bom(io_lib:format("[update error] ~p", [Error])));
        Unhandled ->
            io:format("Unhandled result: ~p~n",[Unhandled]),
            send_chat(To, From,
               strip_bom("Unhandled parse result."))
    end.
