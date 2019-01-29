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
  status_task = ready :: term(),
  last_save_update = {0,0,0},
  buffer = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
%%  snmp:start(),
%%  snmpa:load_mib("MINI-MGW"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

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