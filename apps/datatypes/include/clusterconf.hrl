%% ------------------------------------------------------------------
%% System Configuration types
%% ------------------------------------------------------------------

-type relname() :: atom().
-type hostname() :: string().
-type sys_mgr_name() :: string().
-type node_prefix() :: atom().

%%  Release Configuration.
%%  A relconf gives the release name,
%%  the node name prefix, and the configuration of its applications
%%  different to their defaults.
-type relconf() :: {release, relname(), node_prefix(), [term()]}.

%%  Host configuration.
%%  A hostconf consists of the hostname, a system manager name and 0 or more
%%  relconf terms. The list of relconf terms determines which releases will
%%  be started here.
-type hostconf() :: {host, hostname(), sys_mgr_name(), [relconf()]}.

%% Cluster Configuration.
%% A clustconf consists of 0 or more hostconf terms.
-type clustconf() :: [hostconf()].


%% startup order
-type start_order() :: [{hostname(), sys_mgr_name(), relname()}].
