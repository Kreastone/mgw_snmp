%%%-------------------------------------------------------------------
%% @doc mgw_snmp public API
%% @end
%%%-------------------------------------------------------------------

-module(mgw_snmp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("test~n"),
    snmpc:compile("MINI-MGW"),
    snmpa:load_mib("MINI-MGW"),
    mgw_snmp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
