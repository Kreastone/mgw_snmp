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
-define(TIMEOUT_SAVE, 3000).

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

test() ->
  List_Config = [
    {<<"Version1">>, 1},
    {<<"Version2">>, 1},
    {<<"Version3">>, 1},
    {<<"CommunityRead">>, <<"public">>},
    {<<"CommunityWrite">>, <<"all-rights">>},
    {<<"CommunityTrap">>, <<"private">>},
    {<<"Port">>, 4000},
    {<<"TrapEnable">>, 0},
    {<<"DstIPAddressTrap">>, <<"127.0.0.1">>},
    {<<"DstPortTrap">>, 5000},
    {<<"MaxSize">>, 484}
  ],
  io:format("create list~n"),
  case create_file_config(List_Config) of
    ok ->
      io:format("create files config: ok~n"),
      Res = start_snmp(List_Config),
      io:format("start snmp: ~p~n", [Res]),
      Priv = code:priv_dir(mgw_snmp),
      ResLoad = snmpa:load_mib(Priv ++ "/MINI-MGW"),
      io:format("snmpa:load_mib: ~p~n", [ResLoad]),
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
      snmpa:load_mib(Priv ++ "/MINI-MGW");
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
      List_Indexes = get_list_index(Table_Path),
      {reply, List_Indexes,
        #state{
          status_task = State#state.status_task,
          last_save_update = State#state.last_save_update,
          buffer = State#state.buffer,
          time_update = erlang:now(),
          list_indexes = List_Indexes,
          name_table = Table_Path}}
  end;
handle_call(hibernate, _From, State) ->
  {reply, ok, State, hibernate};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%======================================================================

get_list_index([Start_Path, End_Path]) when is_list(Start_Path), is_list(End_Path) ->
  List_Objects_Parent = snmp_funcs:get_field(Start_Path),
  parse_list_parent_index(lists:reverse(List_Objects_Parent), Start_Path, End_Path, 1, []);
get_list_index(Table_Path) ->
  List_Objects = snmp_funcs:get_field(Table_Path),
  parse_list_index(lists:reverse(List_Objects), 1, []).
parse_list_index([], _Current_Index, Res) ->
  Res;
parse_list_index([Object|Table_Objects], Current_Index, Res) when is_record(Object, object) ->
  parse_list_index(Table_Objects, Current_Index+1, Res ++ [{Current_Index, Object#object.index}]);
parse_list_index(_, _, _) ->
  [].

parse_list_parent_index([], _Start_Path, _End_Path, _Current_Index, Res) ->
  Res;
parse_list_parent_index([Object|Table_Objects], Start_Path, End_Path, Current_Index, Res) when is_record(Object, object) ->
  Parent_Index = Object#object.index,
  List_Objects_Child = snmp_funcs:get_field(Start_Path ++ [list_to_binary(integer_to_list(Parent_Index))] ++ End_Path),
  Result_Parent = get_lists_index(List_Objects_Child, Parent_Index, Current_Index, []),
  parse_list_parent_index(Table_Objects, Start_Path, End_Path, Current_Index + lists:flatlength(Result_Parent), Res ++ Result_Parent );
parse_list_parent_index(_, _, _, _, _) ->
  [].
get_lists_index([], _Parent_Index, _Current_Index, Res) ->
  Res;
get_lists_index([Object|Table_Objects_Child], Parent_Index, Current_Index, Res) when is_record(Object, object) ->
  get_lists_index(Table_Objects_Child, Parent_Index, Current_Index+1, Res ++ [{Current_Index, {Parent_Index, Object#object.index}}]);
get_lists_index(_, _, _, _) ->
  [].

%%======================================================================
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast([set, Address, Name, Value], State) ->
  Buffer = get_buffer(State#state.buffer, Address, Name, Value),
  Status_Task = handle_status(State#state.status_task),
  {noreply,
    #state{
      time_update = State#state.time_update,
      name_table = State#state.name_table,
      list_indexes = State#state.list_indexes,
      status_task = Status_Task,
      last_save_update = erlang:now(),
      buffer = Buffer
    }};
handle_cast(save, State) ->
  Diff_Time = timer:now_diff(erlang:now(), State#state.last_save_update),
  if (Diff_Time < (2000000)) andalso (State#state.status_task == process) ->
      process_wait_save(),
      {noreply, State};
    true ->
      update_all(State#state.buffer),
      {noreply, #state{
        time_update = State#state.time_update,
        name_table = State#state.name_table,
        list_indexes = State#state.list_indexes,
        status_task = ready,
        last_save_update = {0,0,0},
        buffer = []
      }}
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
