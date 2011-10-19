-module(echo_bot).
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

-define(PROCNAME, ejabberd_mod_bot).
-define(BOTNAME, echo_bot).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
        {?MODULE, start_link, [Host, Opts]},
        temporary,
        1000,
        worker,
        [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

init([Host, Opts]) ->
    ?DEBUG("ECHO_BOT: Starting echo_bot", []),
    % add a new virtual host / subdomain "echo".example.com
    MyHost = gen_mod:get_opt_host(Host, Opts, "echo.@HOST@"),
    ejabberd_router:register_route(MyHost, {apply, ?MODULE, route}),
    {ok, Host}.

handle_call(stop, _From, Host) ->
    {stop, normal, ok, Host}.

handle_cast(_Msg, Host) ->
    {noreply, Host}.

handle_info(_Msg, Host) ->
    {noreply, Host}.

terminate(_Reason, Host) ->
    ejabberd_router:unregister_route(Host),
    ok.

code_change(_OldVsn, Host, _Extra) ->
    {ok, Host}.

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

%route(From, #jid{luser = ?BOTNAME} = To, {xmlelement, "message", _, _} = Packet) ->
route(From, To, {xmlelement, "message", _, _} = Packet) ->
    ?INFO_MSG("Sending message ~n~p", [Packet]),
    case xml:get_subtag_cdata(Packet, "body") of
    "" ->
        ok;
    Body ->
        case xml:get_tag_attr_s("type", Packet) of

        "error" ->
            ?ERROR_MSG("Received error message~n~p -> ~p~n~p", [From, To, Packet]);
        _ ->
            echo(To, From, strip_bom(Body))
        end
    end,
    ok;

% don't crash on unhandled messages
route(_, _, _) ->
    ok.


%% HELPER FUNCTIONS

strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).

echo(From, To, Body) ->
    send_message(From, To, "chat", Body).

send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(From, To, XmlBody).
