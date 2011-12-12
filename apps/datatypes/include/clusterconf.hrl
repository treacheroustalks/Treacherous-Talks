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
