%%%-------------------------------------------------------------------
%% @doc mgw_snmp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mgw_snmp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 1000, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},
    Childs = [
        {snmp_genserver, {snmp_genserver, start_link, []},permanent,5000,worker,[snmp_genserver]}
    ],
    {ok, {SupervisorSpecification, Childs}}.

%%====================================================================
%% Internal functions
%%====================================================================
