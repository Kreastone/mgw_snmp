%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Авг. 2018 16:06
%%%-------------------------------------------------------------------
-author("rus").

-type ref_row(_Table) :: Id :: integer().
%%-type mfa() :: #mfa{}.
%%-type constraint() :: #constraint{}.
%%-type parameter() :: #parameter{}.
%%-type object() :: #object{}.

-record(mfa, {
    function :: atom(),
    arity = 0 :: integer(),
    argument = [] :: binary(),
    apply = onRequest :: onStart | onRequest
}).

-record(constraint, {
    type :: string | int | enum | pattern | iso8601 | ip | list,
    range :: undefined | {integer(), integer() | infinity} | [binary()] | [] | binary() | ref_row(parameter) | re:mp() | utc_time | local_time,
    sub_type :: #constraint{}
}).

-record(port_info, {
    link_table :: ref_row(object),
    device :: binary(), %% "eth0"
    type :: switch | phy | cpu_port,
    name_device :: binary(), %% <<"switch0">> | <<>> eth_device
    port :: binary(), %% <<"1">>
    name :: binary() %% <<"LAN 1">>
}).

-record(vlan_info, {
    id :: integer(),
    vid :: pos_integer(), %% 0..4096
    type :: external | internal,
    link_bridge :: ref_row(object),
    switch = <<>> :: binary() %% <<"switch0">> | <<>> for external type
}).

-record(parameter, {
    id :: integer(),
    order = 1 :: integer(),
    object = [] :: [] | integer(),
    name :: binary(),
    value :: undefined | mfa() | binary() | [binary()] | integer() | boolean(),

    notification = 0 :: integer(),  %% TR-069 A.3.2.4
    accessList = [] :: [binary()],     %% TR-069 A.3.2.4

    type = string :: binary(), %string | int | unsignedInt | boolean,
    constraint :: #constraint{},

    access = readOnly :: readOnly | readWrite | writeOnly,
    notification_object :: []
}).

-record(object, {
    id = 0 :: integer(),
    order = 1 :: integer(),
    object = []:: [] | integer(),
    name :: binary(),
    type = single :: single | massif | template | virtual,
    index = 0 :: integer(),
    numberOfEntries = [] :: [] | [binary()] | [ref_row(parameter)], %% decoding into a reference when creating an object
    uniqueParameters = [] :: [binary()], %% checking the child parameters for uniqueness when creating an object

    create_remove :: boolean(),
    on_create_remove :: atom(),
    notification_object :: []
}).


-type udp_sock() :: {inet:socket(), inet:ip_address(), inet:port_number()}.
-type unix_sock():: {inet:socket(), {local, string()}}.

-record(dn_users, {
    id :: mgw_util:id(),
    user :: binary(),
    group :: [integer()],
    type :: mgw_util:type_connection(),
    host = <<"">> :: binary(),
    ttl = 0 :: integer(), %% sek
    socket :: udp_sock() | unix_sock()
}).

-record(http_running, {
    id :: integer(),        % object id from service
    fun_for_run :: function(),
    opt = [] :: ranch_tcp:opts(),
    env = #{} :: cowboy:opts(),
    pid_cowboy :: pid(),
    pid_wait :: pid()
}).