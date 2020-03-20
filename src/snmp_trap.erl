%%%-------------------------------------------------------------------
-module(snmp_trap).
-author("Kreastone").

%% API
-export([coldStart/0]).
-export([warmStart/0]).
-export([alarm_crit_temp/1]).
-export([alarm_crit_temp/2]).
-export([end_alarm_crit_temp/1]).
-export([end_alarm_crit_temp/2]).
-export([lan_change_status_link/1]).
-export([lan_change_status_link/2]).
-export([wan_change_status_link/0]).
-export([wan_change_status_link/1]).
-export([sip_change_register_line/1]).
-export([sip_change_register_line/2]).
-export([sip_change_sip_server/1]).

%% -------------------------------

-define(Sensor(I), [<<"InternetGatewayDevice">>, <<"DeviceInfo">>, <<"TemperatureStatus">>, <<"TemperatureSensor">>, integer_to_binary(I), <<"Value">>]).
-define(LanName(I), [<<"InternetGatewayDevice">>, <<"LANDevice">>, <<"1">>, <<"LANEthernetInterfaceConfig">>, integer_to_binary(I), <<"Name">>]).
-define(LanStatusLink(I), [<<"InternetGatewayDevice">>, <<"LANDevice">>, <<"1">>, <<"LANEthernetInterfaceConfig">>, integer_to_binary(I), <<"Status">>]).
-define(WanStatusLink, [<<"InternetGatewayDevice">>, <<"WANDevice">>, <<"1">>, <<"WANEthernetInterfaceConfig">>, <<"Status">>]).
-define(LineStatus(I), [<<"InternetGatewayDevice">>, <<"Services">>, <<"VoiceService">>, <<"1">>, <<"VoiceProfile">>, <<"1">>, <<"Line">>, integer_to_binary(I), <<"Status">>]).

-define(OidSensor(I), [1,3,6,1,4,1,40248,5,1,2,10,2,1,4,I]).
-define(OidLanName(I), [1,3,6,1,4,1,40248,5,1,9,2,2,1,4,I]).
-define(OidLanStatusLink(I), [1,3,6,1,4,1,40248,5,1,9,2,2,1,3,I]).
-define(OidWanStatusLink, [1,3,6,1,4,1,40248,5,1,10,2,2,0]).
-define(OidChangeServer, [1,3,6,1,4,1,40248,5,2,1,2,4,1,3,1]).
-define(OidLineIndex(I), [1,3,6,1,4,1,40248,5,2,1,2,7,1,1,I]).
-define(OidLineStatus(I), [1,3,6,1,4,1,40248,5,2,1,2,7,1,5,I]).

%% -------------------------------

coldStart() ->
  send_trap(coldStart, []).
warmStart() ->
  send_trap(warmStart, []).

%% -------------------------------

alarm_crit_temp(Index) when is_integer(Index) ->
  send_trap(npoTelecomMgwCriticalTemp, [{?OidSensor(Index), get_field(?Sensor(Index))}]);
alarm_crit_temp(_) ->
  {error, badarg}.

alarm_crit_temp(Index, Value) when is_integer(Index) ->
  send_trap(npoTelecomMgwCriticalTemp, [{?OidSensor(Index), Value}]);
alarm_crit_temp(_, _) ->
  {error, badarg}.

%% --------

end_alarm_crit_temp(Index) when is_integer(Index) ->
  send_trap(npoTelecomMgwEndCriticalTemp, [{?OidSensor(Index), get_field(?Sensor(Index))}]);
end_alarm_crit_temp(_) ->
  {error, badarg}.

end_alarm_crit_temp(Index, Value) when is_integer(Index) ->
  send_trap(npoTelecomMgwEndCriticalTemp, [{?OidSensor(Index), Value}]);
end_alarm_crit_temp(_, _) ->
  {error, badarg}.

%% -------------------------------

lan_change_status_link(Index) when is_integer(Index) ->
  send_trap(npoTelecomMgwEndCriticalTemp, [{?OidLanName(Index), get_field(?LanName(Index))}, {?OidLanStatusLink(Index), get_field(?LanStatusLink(Index))}]);
lan_change_status_link(_) ->
  {error, badarg}.

lan_change_status_link(Index, Value) when is_integer(Index), is_list(Value) ->
  send_trap(npoTelecomMgwChangeStatusLinkLan, [{?OidLanName(Index), get_field(?LanName(Index))}, {?OidLanStatusLink(Index), Value}]);
lan_change_status_link(_, _) ->
  {error, badarg}.

%% -------------------------------

wan_change_status_link() ->
  send_trap(npoTelecomMgwChangeStatusLinkWan, [{?OidWanStatusLink, get_field(?WanStatusLink)}]).

wan_change_status_link(Value) when is_list(Value) ->
  send_trap(npoTelecomMgwChangeStatusLinkWan, [{?OidChangeServer, Value}]);
wan_change_status_link(_) ->
  {error, badarg}.

%% -------------------------------

sip_change_register_line(Index) when is_integer(Index) ->
  send_trap(npoTelecomMgwServiceChangeStatusLine, [{?OidLineIndex(Index), Index}, {?OidLineStatus(Index), get_field(?LineStatus(Index))}]);
sip_change_register_line(_) ->
  {error, badarg}.

sip_change_register_line(Index, Value) when is_integer(Index), is_list(Value) ->
  send_trap(npoTelecomMgwServiceChangeStatusLine, [{?OidLineIndex(Index), Index}, {?OidLineStatus(Index), Value}]);
sip_change_register_line(_, _) ->
  {error, badarg}.

%% -------------------------------

sip_change_sip_server(Value) when is_list(Value) ->
  send_trap(npoTelecomMgwServiceChangeServer, [{?OidWanStatusLink, Value}]);
sip_change_sip_server(_)  ->
  {error, badarg}.

%% ============================
%% Internal function
%% ============================

get_field(Address) ->
  case snmp_funcs:parse_field(access_tables:get_field(Address)) of
    {value, Value} -> Value;
    _ -> 0
  end.

send_trap(Name, VarBinds) ->
  snmpa:send_trap(snmp_master_agent, Name, "standard trap", VarBinds).
