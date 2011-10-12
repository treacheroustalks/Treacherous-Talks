
-module(smtp_frontend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A, Type), {I, {I, start_link, A}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ServerOptions=[[
	{domain, "lin.pcs"}, 
	{address, {0,0,0,0}},
	{port, 25}, 
	{protocol, tcp}, 
	{family, inet}
	]],
    SMTPServer = ?CHILD(gen_smtp_server, [smtp_core, ServerOptions], worker),
    {ok, {{one_for_one, 5, 10}, [SMTPServer]}}.

