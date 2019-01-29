%%%-------------------------------------------------------------------
%% @doc mgw_snmp public API
%% @end
%%%-------------------------------------------------------------------

-module(mgw_snmp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("types.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  case start() of
    ok ->
      snmpa:load_mib("MINI-MGW"),
      mgw_snmp_sup:start_link();
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

%%--------------------------------------------------------------------
stop(_State) ->
  snmp:stop(),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start() -> ok | {error, Reason} when
  Reason :: term().
start() ->
  Address = [<<"InternetGatewayDevice">>, <<"Services">>, <<"SnmpService">>],
  F = fun(Elem, AccIn) ->
    AccIn ++ [{Elem#parameter.name, Elem#parameter.value}]
    end,
  List_Config = access_tables:fold(Address, F, []),

  case create_file_config(List_Config) of
    ok -> start_snmp(List_Config);
    Error ->  Error
  end.

start_snmp(List_Config) ->
  V1 =
    case proplists:get_value(<<"Version1">>, List_Config) of
        1 -> [v1];
        _ -> []
    end,
  V2 =
    case proplists:get_value(<<"Version2">>, List_Config) of
        1 -> [v2];
        _ -> []
    end,
  V3 =
    case proplists:get_value(<<"Version3">>, List_Config) of
        1 -> [v3];
        _ -> []
    end,

  Config = [
    {priority, normal},
    {versions, V1 ++ V2 ++ V3},
    {db_dir, ""},
    {db_init_error, true},
    {mib_storage, [{module,snmpa_mib_storage_ets}]},
    {target_cache, [{verbosity,silence}]},
    {symbolic_store, [{verbosity,silence}]},
    {local_db, [{repair,true},{auto_save,5000},{verbosity,silence}]},
    {error_report_module, snmpa_error_logger},
    {agent_type, master},
    {agent_verbosity, silence},
    {discovery, [{terminating, [{enable, true}, {stage2, discovery}, {trigger_username, ""}]}, {originating, [{enable, true}]}]},
    {config, [{dir, "./tmp/"}, {force_load, true}, {verbosity, silence}]},
    {multi_threaded, false},
    {mib_server, [{mibentry_override,false},{trapentry_override,false},{verbosity,silence},{cache,true}]},
    {note_store, [{timeout,30000},{verbosity,silence}]},
    {net_if, [{module,snmpa_net_if},{verbosity,silence},{options,[{bind_to,false},{no_reuse,false},{req_limit,infinity}]}]}
  ],

  application:set_env(snmp, agent, Config),
  case snmp:start() of
    ok ->
      delete_config_files(),
      ok;
    Error ->
      delete_config_files(),
      Error
  end.

delete_config_files() ->
  Table_Files = [
    "./tmp/agent.conf",
    "./tmp/community.conf",
    "./tmp/context.conf",
    "./tmp/notify.conf",
    "./tmp/standard.conf",
    "./tmp/target_addr.conf",
    "./tmp/target_params.conf",
    "./tmp/usm.conf",
    "./tmp/vacm.conf"
  ],
  delete_file(Table_Files),
  file:del_dir("tmp").

delete_file([]) ->
  ok;
delete_file([FileName|Table]) ->
  file:delete(FileName),
  delete_file(Table).

create_file_config(List_Config) ->
  file:make_dir("tmp"),

  Port = proplists:get_value(<<"Port">>, List_Config),
  MaxSize = proplists:get_value(<<"MaxSize">>, List_Config),
  CommunityRead = binary_to_list(proplists:get_value(<<"CommunityRead">>, List_Config)),
  CommunityWrite = binary_to_list(proplists:get_value(<<"CommunityWrite">>, List_Config)),
  CommunityTrap = binary_to_list(proplists:get_value(<<"CommunityTrap">>, List_Config)),
  ListDstIPTrap = binary_to_list(proplists:get_value(<<"DstIPAddressTrap">>, List_Config)),
  DstPortTrap = proplists:get_value(<<"DstPortTrap">>, List_Config),
  {ok, DstIPTrap} = inet:parse_address(ListDstIPTrap),

  Table_Files = [
    ["./tmp/agent.conf",
      [
        {intAgentUDPPort, Port},
        {intAgentIpAddress, [127,0,1,1]},
        {snmpEngineID, "telecom"},
        {snmpEngineMaxMessageSize, MaxSize}
      ]],
    ["./tmp/community.conf",
      [
        {CommunityRead, CommunityRead, "initial", "", ""},
        {CommunityWrite, CommunityWrite, "initial", "", ""},
        {CommunityTrap, CommunityTrap, "initial", "", ""}
      ]],
    ["./tmp/context.conf",
      [
        ""
      ]],
    ["./tmp/notify.conf",
      [
        {"standard trap", "std_trap", trap}
      ]],
    ["./tmp/standard.conf",
      [
        {sysDescr, "Erlang SNMP agent"},
        {sysObjectID, [1,2,3]},
        {sysContact, "npo-telecom"},
        {sysLocation, "erlang"},
        {sysServices, 72},
        {snmpEnableAuthenTraps, enabled},
        {sysName, "mini-mgw"}
      ]],
    ["./tmp/target_addr.conf",
      [
        {"v3", snmpUDPDomain, {tuple_to_list(DstIPTrap),DstPortTrap}, 1500, 3, "std_trap", "target_v3", "", [], 2048},
        {"v3.3", snmpUDPDomain, {tuple_to_list(DstIPTrap),DstPortTrap}, 1500, 3, "std_inform", "target_v3", "mgrEngine", [], 2048},
        {"v2", snmpUDPDomain, {tuple_to_list(DstIPTrap),DstPortTrap}, 1500, 3, "std_trap", "target_v2", "", [], 2048},
        {"v2.2", snmpUDPDomain, {tuple_to_list(DstIPTrap),DstPortTrap}, 1500, 3, "std_inform", "target_v2", "", [], 2048},
        {"v1", snmpUDPDomain, {tuple_to_list(DstIPTrap),DstPortTrap}, 1500, 3, "std_trap", "target_v1", "", [], 2048}
      ]],
    ["./tmp/target_params.conf",
      [
        {"target_v1", v1, v1, "initial", noAuthNoPriv},
        {"target_v2", v2c, v2c, "initial", noAuthNoPriv},
        {"target_v3", v3, usm, "initial", noAuthNoPriv}
      ]],
    ["./tmp/usm.conf",
      [
        {"kreastone", "initial", "initial", zeroDotZero, usmHMACMD5AuthProtocol, "", "", usmNoPrivProtocol, "", "", "", [43,128,9,152,77,92,193,86,26,72,57,79,14,206,89,20], ""},
        {"kreastone", "templateMD5", "templateMD5", zeroDotZero, usmHMACMD5AuthProtocol, "", "", usmNoPrivProtocol, "", "", "", [43,128,9,152,77,92,193,86,26,72,57,79,14,206,89,20], ""},
        {"kreastone", "templateSHA", "templateSHA", zeroDotZero, usmHMACSHAAuthProtocol, "", "", usmNoPrivProtocol, "", "", "", [17,89,83,210,234,116,161,91,245,239,85,115,253,102,158,149,223,162,49,220], ""}
      ]],
    ["./tmp/vacm.conf",
      [
        {vacmSecurityToGroup, usm, "initial", "initial"},
        {vacmSecurityToGroup, usm, "all-rights", "all-rights"},
        {vacmSecurityToGroup, v2c, "initial", "initial"},
        {vacmSecurityToGroup, v2c, "all-rights", "all-rights"},
        {vacmSecurityToGroup, v1, "initial", "initial"},
        {vacmSecurityToGroup, v1, "all-rights", "all-rights"},
        {vacmAccess, "initial", [], any, noAuthNoPriv, exact, "restricted", [], "restricted"},
        {vacmAccess, "initial", [], usm, authNoPriv, exact, "internet", "internet", "internet"},
        {vacmAccess, "initial", [], usm, authPriv, exact, "internet", "internet", "internet"},
        {vacmAccess, "all-rights", [], any, noAuthNoPriv, exact, "internet", "internet", "internet"},
        {vacmViewTreeFamily, "restricted", [1,3,6,1], included, null},
        {vacmViewTreeFamily, "internet", [1,3,6,1], included, null}
      ]]
  ],
  handle_table_file(Table_Files).

handle_table_file([]) ->
  ok;
handle_table_file([[Path, Table_String]|Table_Files]) ->
  case handle_file(Path, Table_String) of
    ok -> handle_table_file(Table_Files);
    Error -> Error
  end.
handle_file(Path, Table_String) ->
  case file:open(Path, write) of
    {ok, File} ->
      write_file(File, Table_String),
      file:close(File),
      ok;
    _ ->
      Reason = "can't open file " ++ Path,
      {error, Reason}
  end.
write_file(File, []) ->
  file:close(File);
write_file(File, [String|Table_String]) ->
  io:format(File, "~p.~n", [String]),
  write_file(File, Table_String).
