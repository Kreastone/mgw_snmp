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

-include_lib("../mgw_config/include/types.hrl").

%% API
-export([handle_value/2, handle_value/3]).
-export([handle_table/5]).
-export([add_new_entry/3]).
%%
-export([get_field/1, delete_object/1, create_object/1]).

%%----------------------------------------------------------------
%% Instrumentation function for variables.
%% Returns: (get)       {value, Value} | {noValue, noSuchInstance}
%%          (set)       noError | genErr | {noValue, noSuchInstance}
%%----------------------------------------------------------------

-spec handle_value(get, Address) -> {value, Value} | {noValue, noSuchInstance} when
  Address :: [binary()],
  Value   ::  integer() | list().
handle_value(get, Address) ->
  parse_field(get_field(Address));
handle_value(_, _) ->
  {noValue, noSuchInstance}.

%========================================================

-spec handle_value(set, Value, Address) -> noError | {noValue, noSuchInstance} | {error, wrongValue} when
  Value   ::  integer() | list(),
  Address :: [binary()].
handle_value(set, Value, Address) when is_integer(Value) ->
  check_value(Value, Address, get_field(Address));
handle_value(set, Value, Address) when is_list(Value) ->
  check_value(binary:list_to_bin(Value), Address, get_field(Address));
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
    get_indexs(ColIndex, Indexs));
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
      NameIndex, proplists:get_value(NameIndex, Names),
      get_indexs(ColIndex+1, Indexs)) of
    next ->
      handle_table(get_next, [0], [NameIndex+1], Address, Names);
    Value ->
      Value
  end;
handle_table(get_next, _ColIndex, _NameIndex, _Address, _Names) ->
  {noValue, noSuchInstance};

%--------------

handle_table(set, [ColIndex], List_NameValue, Address, Names) when is_list(List_NameValue) ->
  IndexsPath = gen_server:call(snmp_genserver, [get_list_index, Address]),
  Indexs = get_indexs(ColIndex, IndexsPath),
  case check_values(List_NameValue, Indexs, Names) of
    noError ->
      set_table(List_NameValue, Indexs, Names),
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
  create_object(Address),
  noError;
add_new_entry(set, _, _) ->
  {noValue, noSuchInstance}.

%========================================================
% Internal function
%========================================================

get_next_table_value(_ColIndex, _NameIndex, Value_Name, _Indexs) when Value_Name == undefined ->
  [endOfTable];
get_next_table_value(_ColIndex, _NameIndex, _Value_Name, 0) ->
  next;
get_next_table_value(ColIndex, NameIndex, Value_Name, [Indexs, _Path]) when Value_Name == index ->
  [{[NameIndex, ColIndex+1], lists:last(Indexs)}];
get_next_table_value(ColIndex, NameIndex, Value_Name, [Indexs, _Path]) when Value_Name == parent_index ->
  [{[NameIndex, ColIndex+1], lists:nth(erlang:length(Indexs) - 1, Indexs)}];
get_next_table_value(ColIndex, NameIndex, Value_Name, _Indexs) when Value_Name == remove ->
  [{[NameIndex, ColIndex+1], 0}];
get_next_table_value(ColIndex, NameIndex, {add, _}, _Indexs) ->
  [{[NameIndex, ColIndex+1], 0}];
get_next_table_value(ColIndex, NameIndex, Value_Name, Indexs) ->
  case get_table_value(Value_Name, Indexs) of
    [{value, Value}] ->
      [{[NameIndex, ColIndex+1], Value}];
    _ ->
      {noValue, noSuchInstance}
  end.

%--------------

get_table_value(Value_Name, _Indexs) when Value_Name == undefined ->
  {noValue, noSuchInstance};
get_table_value(_Value_Name, 0) ->
  {noValue, noSuchInstance};
get_table_value(Value_Name, [Indexs, _Path]) when Value_Name == index ->
  [{value, lists:last(Indexs)}];
get_table_value(Value_Name, [Indexs, _Path]) when Value_Name == parent_index ->
  [{value, lists:nth(erlang:length(Indexs) - 1, Indexs)}];
get_table_value(Value_Name, _Indexs) when Value_Name == remove ->
  [{value, 0}];
get_table_value({add, _}, _Indexs) ->
  [{value, 0}];
get_table_value(Value_Name, [Indexs, Path]) ->
  Index = lists:last(Indexs),
  Address =  Path ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  case parse_field(get_field(Address)) of
    {value, Value} ->
      [{value, Value}];
    _ ->
      {noValue, noSuchInstance}
  end.

%--------------

set_table([], _Index, _Names) ->
  ok;
set_table([{NameIndex, Value}|Table], [Indexs, Path], Names) ->
  set_table_value(proplists:get_value(NameIndex, Names), lists:last(Indexs), Path, Value),
  set_table(Table, [Indexs, Path], Names).

%--------------

set_table_value(Value_Name, _Index, _Address, _Value) when Value_Name == undefined ->
  {noValue, noSuchInstance};
set_table_value(_Value_Name, Index, _Address, _Value) when Index == 0 ->
  {noValue, noSuchInstance};
set_table_value(Value_Name, _Index, _Address, _Value) when Value_Name == index ->
  {noValue, noSuchInstance};
set_table_value(Value_Name, _Index, _Address, _Value) when Value_Name == parent_index ->
  {noValue, noSuchInstance};
set_table_value(Value_Name, Index, Address, _Value) when Value_Name == remove ->
  delete_object(Address ++ [list_to_binary(integer_to_list(Index))]),
  {noError, 0};
set_table_value({add, Object}, Index, Address, _Value)  ->
  create_object(Address ++ [list_to_binary(integer_to_list(Index))] ++ Object),
  {noError, 0};
set_table_value(Value_Name, Index, Path, Value) ->
  Address =  Path ++ [list_to_binary(integer_to_list(Index))],
  gen_server:cast(snmp_genserver, [set, Address, Value_Name, Value]).

%--------------

check_values([],  _Index, _Names) ->
  noError;
check_values(_List_NameValue,  0, _Names) ->
  genErr;
check_values([{NameIndex, Value}|List_NameValue], [Indexs, Path], Names) ->
  case
    check_value(Value, Path, lists:last(Indexs),
      proplists:get_value(NameIndex, Names)) of
    noError ->
      check_values(List_NameValue, [Indexs, Path], Names);
    _ ->
      genErr
  end.

%--------------

check_value(_Value, _Address, _Index, Value_Name) when Value_Name == undefined  ->
  genErr;
check_value(_Value, _Address, _Index, Value_Name) when Value_Name == remove  ->
  noError;
check_value(_Value, _Address, _Index, Value_Name) when is_tuple(Value_Name)  ->
  noError;
check_value(Value, Address, Index, Value_Name) when is_list(Value) ->
  Path = Address ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  check_value(binary:list_to_bin(Value), Path, get_field(Path));
check_value(Value, Address, Index, Value_Name) when is_integer(Value) ->
  Path = Address ++ [list_to_binary(integer_to_list(Index))] ++ Value_Name,
  check_value(Value, Path, get_field(Path));
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
    if is_binary(X) -> binary:bin_to_list(X) ++ " " ++ ACC;
      is_integer(X) -> integer_to_list(X) ++ " " ++ ACC;
      true -> "undefined " ++ ACC end
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

get_indexs(Index, List_Indexes) ->
  Length = erlang:length(List_Indexes),
  if (Index > Length) -> 0;
    true -> lists:nth(Index, List_Indexes)
  end.

%======================================================

get_field(Address) ->
  access_tables:get_field(Address).

create_object(Address) ->
  access_tables:create_object(Address).

delete_object(Address) ->
  access_tables:delete_object(Address).
