%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Янв. 2019 15:21
%%%-------------------------------------------------------------------
-module(snmp_funcs).
-author("kreastone").

-include("types.hrl").

%% API
-export([handle_value/2, handle_value/3]).
-export([handle_table/5]).
-export([handle_child_table/5]).
-export([add_new_entry/3]).

%%----------------------------------------------------------------
%% Instrumentation function for variables.
%% Returns: (get)       {value, Value} | {noValue, noSuchInstance}
%%          (set)       noError | genErr | {noValue, noSuchInstance}
%%----------------------------------------------------------------

-spec handle_value(get, Address) -> {value, Value} | {noValue, noSuchInstance} when
  Address :: [binary()],
  Value   ::  integer() | list().
handle_value(get, Address) ->
  parse_field(access_tables:get_field(Address));
handle_value(_, _) ->
  {noValue, noSuchInstance}.

%========================================================

-spec handle_value(set, Value, Address) -> noError | {noValue, noSuchInstance} | {error, wrongValue} when
  Value   ::  integer() | list(),
  Address :: [binary()].
handle_value(set, Value, Address) when is_integer(Value) ->
  check_value(Value, Address, access_tables:get_field(Address));
handle_value(set, Value, Address) when is_list(Value) ->
  check_value(binary:list_to_bin(Value), Address, access_tables:get_field(Address));
handle_value(set, _, _) ->
  {noValue, noSuchInstance}.

%%----------------------------------------------------------------
%% Instrumentation function for tables.
%% Returns: (get)       [{value, Value}] | {noValue, noSuchInstance}
%%          (get_next)  [{[Row, Col], Value}] | {noValue, noSuchInstance}
%%          (set)       {noError, 0} | {genErr, 0} | {noValue, noSuchInstance}
%%----------------------------------------------------------------

-spec handle_table(Fun, ColIndex, NameIndex, Address, Names) -> [{value, Value}] | {noValue, noSuchInstance} when
  Fun :: get | get_next | set,
  ColIndex :: [integer()],
  NameIndex :: [integer()],
  Address :: [binary()],
  Names :: [tuple()],
  Value   ::  integer() | list().
handle_table(get, [ColIndex], [NameIndex], Address, Names) ->
  Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  get_table_value(
    proplists:get_value(NameIndex, Names),
    get_index(ColIndex, Indexs),
    Address);
handle_table(get, _, _, _Start_Path, _Names) ->
  {noValue, noSuchInstance};

%--------------

handle_table(get_next, [], [NameIndex], Address, Names) ->
  handle_table(get_next, [0], [NameIndex], Address, Names);
handle_table(get_next, [ColIndex], [NameIndex], Address, Names) when NameIndex == 0 ->
  handle_table(get_next, [ColIndex], [NameIndex + 1], Address, Names);
handle_table(get_next, [ColIndex], [NameIndex], Address, Names) ->
  Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  case
    get_next_table_value(ColIndex,
      NameIndex, Address,
      proplists:get_value(NameIndex, Names),
      get_index(ColIndex+1, Indexs)) of
    next ->
      handle_table(get_next, [0], [NameIndex+1], Address, Names);
    Value ->
      Value
  end;
handle_table(get_next, _ColIndex, _NameIndex, _Address, _Names) ->
  {noValue, noSuchInstance};

%--------------

handle_table(set, [ColIndex], List_NameValue, Address, Names) when is_list(List_NameValue) ->
  Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  Index = get_index(ColIndex, Indexs),
  case check_values(List_NameValue,
    Index,
    Address, Names) of
    noError ->
      set_table(List_NameValue, Index, Address, Names),
      {noError, 0};
    _ ->
      {genErr, 0}
  end;
handle_table(set, _, _, _, _) ->
  {genErr, 0}.

%%----------------------------------------------------------------
%% Instrumentation function for add new entry to table.
%% Returns: (set)       noError | {noValue, noSuchInstance}
%%----------------------------------------------------------------

-spec add_new_entry(set, _Value, Address) -> noError | {noValue, noSuchInstance} when
  _Value  :: integer(),
  Address :: [binary()].
add_new_entry(set, _Value, Address) ->
  access_tables:create_object(Address),
  noError;
add_new_entry(set, _, _) ->
  {noValue, noSuchInstance}.

%%----------------------------------------------------------------
%% CHILD TABLES
%%----------------------------------------------------------------

-spec handle_child_table(Fun, ColIndex, NameIndex, Address, Names) -> [{value, Value}] | {noValue, noSuchInstance} when
  Fun :: get | get_next | set,
  ColIndex :: [integer()],
  NameIndex :: [integer()],
  Address :: [binary()],
  Names :: [tuple()],
  Value   ::  integer() | list().
handle_child_table(get, [ColIndex], [NameIndex], Address, Names) ->
  Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  get_child_table_value(
    proplists:get_value(NameIndex, Names),
    get_indexs(ColIndex, Indexs),
    Address);
handle_child_table(get, _, _, _, _) ->
  {noValue, noSuchInstance};

%--------------

handle_child_table(get_next, [], [NameIndex], Address, Names) ->
  handle_child_table(get_next, [0], [NameIndex], Address, Names);
handle_child_table(get_next, [ColIndex], [NameIndex], Address, Names) when NameIndex == 0 ->
  handle_child_table(get_next, [ColIndex], [NameIndex + 1], Address, Names);
handle_child_table(get_next, [ColIndex], [NameIndex], Address, Names) ->
  Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  case
    get_next_child_table_value(ColIndex,
      NameIndex, Address,
      proplists:get_value(NameIndex, Names),
      get_indexs(ColIndex+1, Indexs)) of
    next ->
      handle_child_table(get_next, [0], [NameIndex+1], Address, Names);
    Value ->
      Value
  end;
handle_child_table(get_next, _ColIndex, _NameIndex, _Address, _Names) ->
  {noValue, noSuchInstance};

handle_child_table(set, [ColIndex], List_NameValue, Address, Names) when is_list(List_NameValue) ->
  All_Indexs = gen_server:call(snmp_genserver, [get_list_index, Address]),
  Index = get_indexs(ColIndex, All_Indexs),

  case check_child_values(List_NameValue,
    Index,
    Address, Names) of
    noError ->
      set_table(List_NameValue, Index, Address, Names),
      {noError, 0};
    _ ->
      {genErr, 0}
  end;
handle_child_table(set, _, _, _, _) ->
  {genErr, 0}.

%========================================================
% Internal function
%========================================================

get_next_child_table_value(_ColIndex, _NameIndex, _Address, Value_Name, _Index) when Value_Name == undefined ->
  [endOfTable];
get_next_child_table_value(_ColIndex, _NameIndex, _Address, _Value_Name, {Parent_Index, Index}) when Parent_Index == 0, Index == 0 ->
  next;
get_next_child_table_value(ColIndex, NameIndex, _Address, Value_Name, {_Parent_Index, Index}) when Value_Name == index ->
  [{[NameIndex, ColIndex+1], Index}];
get_next_child_table_value(ColIndex, NameIndex, _Address, Value_Name, {Parent_Index, _Index}) when Value_Name == parent_index ->
  [{[NameIndex, ColIndex+1], Parent_Index}];
get_next_child_table_value(ColIndex, NameIndex, _Address, Value_Name, _Index) when Value_Name == remove ->
  [{[NameIndex, ColIndex+1], 0}];
get_next_child_table_value(ColIndex, NameIndex, [Start_Address, End_Address], Value_Name, {Parent_Index, Index}) ->
  Address =  Start_Address ++ [list_to_binary(integer_to_list(Parent_Index))] ++ End_Address,
  case get_table_value(Value_Name, Index, Address) of
    [{value, Value}] ->
      [{[NameIndex, ColIndex+1], Value}];
    _ ->
      {noValue, noSuchInstance}
  end.

%--------------

get_child_table_value(Value_Name, _Indexs, _Address) when Value_Name == undefined ->
  {noValue, noSuchInstance};
get_child_table_value(_Value_Name, {Parent_Index, Index}, _Address) when Index == 0, Parent_Index == 0 ->
  {noValue, noSuchInstance};
get_child_table_value(Value_Name, {_Parent_Index, Index}, _Address) when Value_Name == index ->
  [{value, Index}];
get_child_table_value(Value_Name, {Parent_Index, _Indexs}, _Address) when Value_Name == parent_index ->
  [{value, Parent_Index}];
get_child_table_value(Value_Name, _Indexs, _Address) when Value_Name == remove ->
  [{value, 0}];
get_child_table_value(Value_Name, {Parent_Index, Index}, [Start_Address, End_Address]) ->
  Address =  Start_Address ++ [list_to_binary(integer_to_list(Parent_Index))] ++ End_Address ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  case parse_field(access_tables:get_field(Address)) of
    {value, Value} ->
      [{value, Value}];
    _ ->
      {noValue, noSuchInstance}
  end.

%--------------

set_table([], _Index, _Address, _Names) ->
  ok;
set_table([{NameIndex, Value}|Table], {Parent_Index, Index}, [Start_Address, End_Address], Names) ->
  Address = Start_Address ++ [list_to_binary(integer_to_list(Parent_Index))] ++ End_Address,
  set_table_value(proplists:get_value(NameIndex, Names), Index, Address, Value),
  set_table(Table, {Parent_Index, Index}, [Start_Address, End_Address], Names);
set_table([{NameIndex, Value}|Table], Index, Address, Names) ->
  set_table_value(proplists:get_value(NameIndex, Names), Index, Address, Value),
  set_table(Table, Index, Address, Names).

set_table_value(Value_Name, _Index, _Address, _Value) when Value_Name == undefined ->
  {noValue, noSuchInstance};
set_table_value(_Value_Name, Index, _Address, _Value) when Index == 0 ->
  {noValue, noSuchInstance};
set_table_value(Value_Name, _Index, _Address, _Value) when Value_Name == index ->
  {noValue, noSuchInstance};
set_table_value(Value_Name, Index, Address, _Value) when Value_Name == remove ->
  access_tables:delete_object(Address ++ [list_to_binary(integer_to_list(Index))]),
  {noError, 0};
set_table_value({add, Object}, Index, Address, _Value)  ->
  access_tables:create_object(Address ++ [list_to_binary(integer_to_list(Index))] ++ Object),
  {noError, 0};
set_table_value(Value_Name, Index, Path, Value) ->
  Address =  Path ++ [list_to_binary(integer_to_list(Index))],
  gen_server:cast(snmp_genserver, [set, Address, Value_Name, Value]).

%--------------

get_next_table_value(_ColIndex, _NameIndex, _Address, Value_Name, _Index) when Value_Name == undefined ->
  [endOfTable];
get_next_table_value(_ColIndex, _NameIndex, _Address, _Value_Name, Index) when Index == 0 ->
  next;
get_next_table_value(ColIndex, NameIndex, _Address, Value_Name, Index) when Value_Name == index ->
  [{[NameIndex, ColIndex+1], Index}];
get_next_table_value(ColIndex, NameIndex, _Address, Value_Name, _Index) when Value_Name == remove ->
  [{[NameIndex, ColIndex+1], 0}];
get_next_table_value(ColIndex, NameIndex, _Address, Value_Name, _Index) when is_tuple(Value_Name) ->
  [{[NameIndex, ColIndex+1], 0}];
get_next_table_value(ColIndex, NameIndex, Address, Value_Name, Index) ->
  case get_table_value(Value_Name, Index, Address) of
    [{value, Value}] ->
      [{[NameIndex, ColIndex+1], Value}];
    _ ->
      {noValue, noSuchInstance}
  end.

%--------------

get_table_value(Value_Name, _Index, _Address) when Value_Name == undefined ->
  {noValue, noSuchInstance};
get_table_value(_Value_Name, Index, _Address) when Index == 0 ->
  {noValue, noSuchInstance};
get_table_value(Value_Name, Index, _Address) when Value_Name == index ->
  [{value, Index}];
get_table_value(Value_Name, _Index, _Address) when Value_Name == remove ->
  [{value, 0}];
get_table_value(Value_Name, _Index, _Address) when is_tuple(Value_Name) ->
  [{value, 0}];
get_table_value(Value_Name, Index, Path) ->
  Address =  Path ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  case parse_field(access_tables:get_field(Address)) of
    {value, Value} ->
      [{value, Value}];
    _ ->
    {noValue, noSuchInstance}
  end.

%--------------

check_child_values([],  _Index, _Address, _Names) ->
  noError;
check_child_values(_List_NameValue,  {Parent_Index, Index}, _Address, _Names) when Parent_Index == 0, Index == 0 ->
  genErr;
check_child_values([{NameIndex, Value}|List_NameValue], {Parent_Index, Index}, [Start_Address, End_Address], Names) ->
  Address = Start_Address ++ [list_to_binary(integer_to_list(Parent_Index))] ++ End_Address,
  case
    check_value(Value, Address, Index,
      proplists:get_value(NameIndex, Names)) of
    noError ->
      check_child_values(List_NameValue, {Parent_Index, Index}, [Start_Address, End_Address], Names);
    _ ->
      genErr
  end.

%--------------

check_values([], _Index, _Address, _Names) ->
  noError;
check_values(_List_NameValue, Index, _Address, _Names) when Index == 0 ->
  genErr;
check_values([{NameIndex, Value}|List_NameValue], Index, Address, Names) ->
  case
    check_value(Value, Address, Index,
      proplists:get_value(NameIndex, Names)) of
    noError ->
      check_values(List_NameValue, Index, Address, Names);
    _ ->
      genErr
  end.

check_value(_Value, _Address, _Index, Value_Name) when Value_Name == undefined  ->
  genErr;
check_value(_Value, _Address, _Index, Value_Name) when Value_Name == remove  ->
  noError;
check_value(_Value, _Address, _Index, Value_Name) when is_tuple(Value_Name)  ->
  noError;
check_value(Value, Address, Index, Value_Name) when is_list(Value) ->
  Path = Address ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  check_value(binary:list_to_bin(Value), Path, access_tables:get_field(Path));
check_value(Value, Address, Index, Value_Name) when is_integer(Value) ->
  Path = Address ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  check_value(Value, Path, access_tables:get_field(Path));
check_value(_Value, _Address, _Index, _Value_Name) ->
  genErr.

%--------------

check_value(Value, Address, Elem) when is_record(Elem, parameter) ->
  case types_constraint:check(Value, Elem#parameter.constraint) of
    true ->
      Name = lists:last(Address),
      Path = Address -- [Name],
      gen_server:cast(snmp_genserver, [set, Path, Name, Value]),
      noError;
    false ->
      genErr
  end;
check_value(_, _, _) ->
  {noValue, noSuchInstance}.

%--------------

parse_field(Elem) when is_record(Elem, parameter), Elem#parameter.type == <<"string">> ->
  {value, parse_string_field(Elem)};
parse_field(Elem) when is_record(Elem, parameter), Elem#parameter.type == <<"int">> ->
  {value, parse_integer_field(Elem)};
parse_field(Elem) when is_record(Elem, parameter), Elem#parameter.type == <<"unsignedInt">> ->
  {value, parse_integer_field(Elem)};
parse_field(Elem) when is_record(Elem, parameter), Elem#parameter.type == <<"boolean">> ->
  {value, parse_boolean_field(Elem)};
parse_field(_) ->
  {noValue, noSuchInstance}.

%--------------

parse_integer_field(Elem) when Elem#parameter.value == undefined ->
  0;
parse_integer_field(Elem) when Elem#parameter.value == <<>> ->
  0;
parse_integer_field(Elem) when is_record(Elem#parameter.value, mfa) ->
  mfa:apply(Elem);
parse_integer_field(Elem) when is_integer(Elem#parameter.value) ->
  Elem#parameter.value;
parse_integer_field(_) ->
  0.

%--------------

parse_string_field(Elem) when Elem#parameter.value == undefined ->
  "undefined";
parse_string_field(Elem) when is_record(Elem#parameter.value, mfa) ->
  Val = mfa:apply(Elem),
  binary:bin_to_list(Val);
parse_string_field(Elem) when is_list(Elem#parameter.value) ->
  lists:foldl(fun(X, ACC) ->
    binary:bin_to_list(X) ++ " " ++ ACC
              end, [], Elem#parameter.value);
parse_string_field(Elem) when Elem#parameter.value == <<>> ->
  " ";
parse_string_field(Elem) when is_binary(Elem#parameter.value) ->
  binary:bin_to_list(Elem#parameter.value);
parse_string_field(_) ->
  "unknown error".

%--------------

parse_boolean_field(Elem) when is_record(Elem, parameter), Elem#parameter.value == undefined ->
  0;
parse_boolean_field(Elem) when is_record(Elem, parameter), is_record(Elem#parameter.value, mfa) ->
  mfa:apply(Elem);
parse_boolean_field(Elem) when is_record(Elem, parameter), Elem#parameter.value >= 0, Elem#parameter.value =< 1 ->
  Elem#parameter.value;
parse_boolean_field(_) ->
  0.

%--------------

get_index(Index, List_Indexes) ->
  case proplists:get_value(Index, List_Indexes) of
    undefined -> 0;
    Value -> Value
  end.
get_indexs(Index, List_Indexes) ->
  case proplists:get_value(Index, List_Indexes) of
    undefined -> {0, 0};
    {Parent_Index, New_Index} -> {Parent_Index, New_Index};
    _ -> {0, 0}
  end.

%======================================================