%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Янв. 2019 16:14
%%%-------------------------------------------------------------------
-module(snmp_genserver).
-author("kreastone").
-behaviour(gen_server).

-include("types.hrl").

%% API
-export([start_link/0]).
-export([apply_setting/0]).
-export([start_listen/0, stop_listen/0]).
-export([test/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT_SAVE, 5000).

-record(state, {
  time_update = {0,0,0},
  name_table :: binary(),
  list_indexes = [],
%------
  status_task = ready :: ready | process,
  last_save_update = {0,0,0},
  buffer = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

apply_setting() ->
    snmp:stop(),
    start_listen().

test() ->
  List_Config = [
    {<<"SnmpEnable">>, true},
    {<<"Version1">>, true},
    {<<"Version2">>, true},
    {<<"Version3">>, true},
    {<<"CommunityRead">>, <<"public">>},
    {<<"CommunityWrite">>, <<"all-rights">>},
    {<<"CommunityTrap">>, <<"private">>},
    {<<"Port">>, 4000},
    {<<"TrapEnable">>, false},
    {<<"DstIPAddressTrap">>, <<"127.0.0.1">>},
    {<<"DstPortTrap">>, 5000},
    {<<"MaxSize">>, 484},
    {<<"UsmUser">>, <<"initial">>},
    {<<"AuthPriv">>, <<"no auth no priv">>}, %% no auth no priv | auth no priv | auth priv
    {<<"AuthProtocol">>, <<"md5">>},  %% md5 | sha
    {<<"AuthKey">>, <<"">>},
    {<<"PrivProtocol">>, <<"des">>},  %% des | aes
    {<<"PrivKey">>, <<"">>}
  ],

  io:format("create list~n"),
  case create_file_config(List_Config) of
    ok ->
      io:format("create files config: ok~n"),
      Res = start_snmp(List_Config),
      io:format("start snmp: ~p~n", [Res]),
      Priv = code:priv_dir(mgw_snmp),
      snmpa:load_mib(Priv ++ "/MINI-MGW"),
      snmpa:load_mib(Priv ++ "/MINI-MGW-SIP"),
      io:format("community read: public~n"),
      io:format("community write: all-right~n"),
      io:format("port: 4000~n");
  Error ->
      io:format("create files config: error, ~p~n", [Error])
  end.

start_listen() ->
  case start() of
    ok ->
%%      Priv = code:priv_dir(mgw_snmp),
%%      case filelib:is_file("MINI-MGW.bin") of
%%        true -> ok;
%%        false -> snmpc:compile(Priv ++ "/MINI-MGW")
%%      end,
      Priv = code:priv_dir(mgw_snmp),
      snmpa:load_mib(Priv ++ "/MINI-MGW"),
      snmpa:load_mib(Priv ++ "/MINI-MGW-SIP");
    not_start ->
      [];
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

%%%===================================================================

stop_listen() ->
  snmp:stop(),
  gen_server:call(?MODULE, hibernate).

%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  Priv = code:priv_dir(mgw_snmp),
  code:add_pathsa([Priv ++ "/snmp/ebin"]),
  code:add_path(Priv ++ "/snmp"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}, hibernate}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call([get_list_index, Table_Path], _From, State) ->
  Diff_Time = timer:now_diff(erlang:now(), State#state.time_update),
  if
    (Diff_Time < 2000000) andalso (State#state.name_table == Table_Path) ->
      {reply, State#state.list_indexes, State};
    true ->
      List_Indexes = get_list_index_all(Table_Path, []),
      {reply, List_Indexes,
        State#state{
          time_update = erlang:now(),
          list_indexes = List_Indexes,
          name_table = Table_Path
        }}
  end;
handle_call(hibernate, _From, State) ->
  {reply, ok, State, hibernate};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%======================================================================

get_list_index_all([], Res) ->
  Res;
get_list_index_all([Path|Table_Path], Res) ->
  case get_res_index(Path, Res) of
    [] -> [];
    New_Res -> get_list_index_all(Table_Path, New_Res)
  end.

get_res_index(Path, []) ->
  List_Objects = snmp_funcs:get_field(Path),
  parse_index_first(List_Objects, Path, []);
get_res_index(Path, Res) ->
  parse_index_next(Res, Path, []).

parse_index_first([], _, Res) ->
  lists:sort(Res);
parse_index_first([Object|Objects], Path, Res) when is_record(Object, object) ->
  Index = Object#object.index,
  parse_index_first(Objects, Path, Res ++ [[[Index], Path]]);
parse_index_first(_, _, _) ->
  [].

parse_index_next([], _, Res) ->
  lists:sort(Res);
parse_index_next([[Cur_Index, Cur_Path]|Path_Index], Path, Res) ->
  Address = Cur_Path ++ [list_to_binary(integer_to_list(lists:last(Cur_Index)))] ++ Path,
  List_Objects = snmp_funcs:get_field(Address),
  New_Res = get_new_res(List_Objects, Address, Cur_Index, []),
  parse_index_next(Path_Index, Path, Res ++ New_Res).

get_new_res([], _, _, Res) ->
  Res;
get_new_res([Object|Objects], Address, Cur_Index, Res) when is_record(Object, object) ->
  Index = Object#object.index,
  New_Index = Cur_Index ++ [Index],
  New_Res = Res ++ [[New_Index, Address]],
  get_new_res(Objects, Address, Cur_Index, New_Res).

%%======================================================================
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast([set, Address, Name, Value], State) ->
  Buffer = get_buffer(State#state.buffer, Address, Name, Value),
  Status_Task = handle_status(State#state.status_task),
  {noreply,
    State#state{
      status_task = Status_Task,
      last_save_update = erlang:now(),
      buffer = Buffer}};
handle_cast(save, State) ->
  Diff_Time = timer:now_diff(erlang:now(), State#state.last_save_update),
  if (Diff_Time < (2000000)) andalso (State#state.status_task == process) ->
      process_wait_save(),
      {noreply, State};
    true ->
      update_all(State#state.buffer),
      {noreply,
        State#state{
          status_task = ready,
          last_save_update = {0,0,0},
          buffer = []}}
  end;
handle_cast(_Request, State) ->
  {noreply, State}.

%%======================================================================
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%======================================================================
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%======================================================================
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_buffer(Buffer, Address, Name, Value) ->
  case proplists:get_value(Address, Buffer) of
    undefined -> Buffer ++ [{Address, [{Name, Value}]}];
    Table_Name_Value ->
      proplists:delete(Address, Buffer) ++ [{Address,  get_name_value(Table_Name_Value, Name, Value)}]
  end.
get_name_value(Table, Name, Value) ->
  case proplists:get_value(Name, Table) of
    undefined -> Table ++ [{Name, Value}];
    _ ->
      proplists:delete(Name, Table) ++ [{Name, Value}]
  end.

handle_status(Status) ->
  case Status of
    ready ->
      process_wait_save(),
      process;
    Status ->
      Status
  end.

process_wait_save() ->
  F = fun() ->
        timer:sleep(?TIMEOUT_SAVE),
        gen_server:cast(snmp_genserver, save)
      end,
  erlang:spawn(F).

update_all([]) ->
  ok;
update_all([{Address, Table_Name_Value}|Table]) ->
  F = fun(Elem) ->
        case proplists:get_value(Elem#parameter.name, Table_Name_Value) of
          undefined -> [];
          Value -> Elem#parameter{value = Value}
        end
      end,
  access_tables:update(Address, F),
  update_all(Table).

%%====================================================================
%% Start snmp server
%%====================================================================

-spec start() -> ok | {error, Reason} when
  Reason :: term().
start() ->
  Address = [<<"InternetGatewayDevice">>, <<"Services">>, <<"X_1CA0D3_SnmpService">>],
  F = fun(Elem, AccIn) ->
    AccIn ++ [{Elem#parameter.name, Elem#parameter.value}]
      end,
  List_Config = access_tables:fold(Address, F, []),

  case proplists:get_value(<<"SnmpEnable">>, List_Config) of
    true ->
      case create_file_config(List_Config) of
        ok -> start_snmp(List_Config);
        Error ->  Error
      end;
    _ ->
      not_start
  end.

start_snmp(List_Config) ->
  V1 =
    case proplists:get_value(<<"Version1">>, List_Config) of
      true -> [v1];
      _ -> []
    end,
  V2 =
    case proplists:get_value(<<"Version2">>, List_Config) of
      true -> [v2];
      _ -> []
    end,
  V3 =
    case proplists:get_value(<<"Version3">>, List_Config) of
      true -> [v3];
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
  UsmUser = binary_to_list(proplists:get_value(<<"UsmUser">>, List_Config)),
  AuthPriv = proplists:get_value(<<"AuthPriv">>, List_Config),
  AuthProtocol = proplists:get_value(<<"AuthProtocol">>, List_Config),
  AuthKeyString = binary_to_list(proplists:get_value(<<"AuthKey">>, List_Config)),
  PrivProtocol = proplists:get_value(<<"PrivProtocol">>, List_Config),
  PrivKeyString = binary_to_list(proplists:get_value(<<"PrivKey">>, List_Config)),

  {AP, A, AKey, P, PKey} =
    case {AuthPriv, AuthProtocol, PrivProtocol} of
      {<<"no auth no priv">>, _, _} -> {noAuthNoPriv, usmNoAuthProtocol, "", usmNoPrivProtocol, ""};
      {<<"auth no priv">>, <<"md5">>, _} -> {authNoPriv, usmHMACMD5AuthProtocol, snmp_usm:passwd2localized(md5, AuthKeyString, "telecom"), usmNoPrivProtocol, ""};
      {<<"auth no priv">>, <<"sha">>, _} -> {authNoPriv, usmHMACSHAAuthProtocol, snmp_usm:passwd2localized(sha, AuthKeyString, "telecom"), usmNoPrivProtocol, ""};
      {<<"auth priv">>, <<"md5">>, <<"des">>} -> {authPriv, usmHMACMD5AuthProtocol, snmp_usm:passwd2localized(md5, PrivKeyString, "telecom"), usmDESPrivProtocol, snmp_usm:passwd2localized(md5, "telecom", PrivKeyString)};
      {<<"auth priv">>, <<"md5">>, <<"aes">>} -> {authPriv, usmHMACMD5AuthProtocol, snmp_usm:passwd2localized(md5, PrivKeyString, "telecom"), usmDESPrivProtocol, snmp_usm:passwd2localized(md5, "telecom", PrivKeyString)};
      _ -> {noAuthNoPriv, usmNoAuthProtocol, "", usmNoPrivProtocol, ""}
    end,

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
        {CommunityWrite, CommunityWrite, CommunityWrite, "", ""},
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
        {"telecom", UsmUser, UsmUser, zeroDotZero, A, "", "", P, "", "", "", AKey, PKey}
      ]],
    ["./tmp/vacm.conf",
      [
        {vacmSecurityToGroup, usm, UsmUser, UsmUser},
        {vacmSecurityToGroup, v2c, "initial", "initial"},
        {vacmSecurityToGroup, v2c, CommunityWrite, CommunityWrite},
        {vacmSecurityToGroup, v1, "initial", "initial"},
        {vacmSecurityToGroup, v1, CommunityWrite, CommunityWrite},
        {vacmAccess, UsmUser, [], any, AP, exact, "internet", [], "internet"},
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
