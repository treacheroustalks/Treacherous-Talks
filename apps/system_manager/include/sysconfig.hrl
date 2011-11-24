%% ------------------------------------------------------------------
%% System Configuration types
%% ------------------------------------------------------------------

-type relname() :: atom().
-type hostname() :: string().

%%  Release Configuration.
%%  A relconf gives the release name and the configuration of its applications
%%  different to their defaults.
-type relconf() :: {release, relname(), [term()]}.

%%  Host configuration.
%%  A hostconf consists of the hostname and 0 or more relconf terms.
%%  The list of relconf terms determines which releases will be started here.
-type hostconf() :: {host, hostname(), [relconf()]}.

%% System Configuration.
%% A sysconf consists of 0 or more hostconf terms.
-type sysconf() :: [hostconf()].
