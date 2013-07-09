%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   8 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_uri).

-include("redis_node.hrl").

%% API
-export([new/4, get/2, from_string/1, to_string/1]).

-record(redis_node_uri, {
    password = ""          :: list(),
    host     = "127.0.0.1" :: list(),
    port     = 6379        :: integer(),
    database = 0           :: integer()
}).

-type obj() :: #redis_node_uri{}.
-export_type([obj/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

new(Password, Host, Port, Database) ->
    #redis_node_uri{password=Password, host=Host, port=Port, database=Database}.

get(List, Record) when is_list(List) ->
    [g(Atom, Record) || Atom <- List];
get(Atom, Record) when is_atom(Atom) ->
    g(Atom, Record).

from_string(Uri) when is_list(Uri) ->
    from_string(iolist_to_binary(Uri));
from_string(<<>>) ->
    #redis_node_uri{};
from_string(<< "redis://", Uri/binary >>) ->
    from_string(Uri);
from_string(Uri) when is_binary(Uri) ->
    {Password, Uri1} = parse_password(Uri),
    {Host, Port, Uri2} = parse_host_port(Uri1, <<>>),
    {Database, _Uri3} = parse_database(Uri2, <<>>),
    new(Password, Host, Port, Database).

to_string(#redis_node_uri{password="", host=Host, port=Port, database=Database}) ->
    lists:flatten(io_lib:format("redis://~s:~p/~p", [Host, Port, Database]));
to_string(#redis_node_uri{password=Password, host=Host, port=Port, database=Database}) ->
    lists:flatten(io_lib:format("redis://:~s@~s:~p/~p", [Password, Host, Port, Database])).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

g(password, #redis_node_uri{password=Ret}) -> Ret;
g(host, #redis_node_uri{host=Ret}) -> Ret;
g(port, #redis_node_uri{port=Ret}) -> Ret;
g(database, #redis_node_uri{database=Ret}) -> Ret.

parse_password(<< $:, Uri/binary >>) ->
    parse_password(Uri, <<>>);
parse_password(Uri) ->
    {"", Uri}.

parse_password(<<>>, Password) ->
    {binary_to_list(Password), <<>>};
parse_password(<< $@, Uri/binary >>, Password) ->
    {binary_to_list(Password), Uri};
parse_password(<< C, Uri/binary >>, Password) ->
    parse_password(Uri, << Password/binary, C >>).

parse_host_port(<<>>, Host) ->
    {Port, Uri} = parse_port(<<>>, <<>>),
    {binary_to_list(Host), Port, Uri};
parse_host_port(<< $/, Uri/binary >>, Host) ->
    {Port, _} = parse_port(<<>>, <<>>),
    {binary_to_list(Host), Port, Uri};
parse_host_port(<< $:, Uri/binary >>, Host) ->
    {Port, Uri2} = parse_port(Uri, <<>>),
    {binary_to_list(Host), Port, Uri2};
parse_host_port(<< $[, Uri/binary >>, Host) ->
    {Host2, Uri2} = parse_ipv6_host(<< $[, Uri/binary >>, Host),
    parse_host_port(Uri2, Host2);
parse_host_port(<< C, Uri/binary >>, Host) ->
    parse_host_port(Uri, << Host/binary, C >>).

parse_ipv6_host(<<>>, Host) ->
    {Host, <<>>};
parse_ipv6_host(<< $], Uri/binary >>, Host) ->
    {<< Host/binary, $] >>, Uri};
parse_ipv6_host(<< C, Uri/binary >>, Host) ->
    parse_ipv6_host(Uri, << Host/binary, C >>).

parse_port(<<>>, <<>>) ->
    {6379, <<>>};
parse_port(<<>>, Port) ->
    {binary_to_integer(Port), <<>>};
parse_port(<< $/, Uri/binary >>, <<>>) ->
    {6379, Uri};
parse_port(<< $/, Uri/binary >>, Port) ->
    {binary_to_integer(Port), Uri};
parse_port(<< C, Uri/binary >>, Port) ->
    parse_port(Uri, << Port/binary, C >>).

parse_database(<<>>, <<>>) ->
    {0, <<>>};
parse_database(<<>>, Database) ->
    {binary_to_integer(Database), <<>>};
parse_database(<< C, Uri/binary >>, Database) ->
    parse_database(Uri, << Database/binary, C >>).

%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

-ifdef(TEST).

get_test_() ->
    URI = #redis_node_uri{password="password", host="host", port=1234, database=5},
    Tests = [
        {password, "password"},
        {host, "host"},
        {port, 1234},
        {database, 5},
        {[password, host, port, database], ["password", "host", 1234, 5]}
    ],
    [{lists:flatten(io_lib:format("~p", [{Rt, Rs}])),
        fun() -> Rs = get(Rt, URI) end} || {Rt, Rs} <- Tests].

from_string_test_() ->
    %% {URI, Result}
    Tests = [
        {"redis://:password@host:1234/5", #redis_node_uri{password="password", host="host", port=1234, database=5}},
        {":password@host:1234/5", #redis_node_uri{password="password", host="host", port=1234, database=5}},
        {"host:1234/5", #redis_node_uri{host="host", port=1234, database=5}},
        {"host:1234", #redis_node_uri{host="host", port=1234}},
        {"host/5", #redis_node_uri{host="host", database=5}},
        {"host:/5", #redis_node_uri{host="host", database=5}},
        {"", #redis_node_uri{}},
        {"redis://", #redis_node_uri{}},
        {"redis://[fe80::1]:1234/5", #redis_node_uri{host="[fe80::1]", port=1234, database=5}},
        {"redis://[fe80::1]", #redis_node_uri{host="[fe80::1]"}},
        {"[fe80::1]", #redis_node_uri{host="[fe80::1]"}}
    ],
    [{lists:flatten(io_lib:format("~p", [Rt])),
        fun() -> Rs = from_string(Rt) end} || {Rt, Rs} <- Tests].

to_string_test_() ->
    %% {URI, Result}
    Tests = [
        {#redis_node_uri{password="password", host="host", port=1234, database=5}, "redis://:password@host:1234/5"},
        {#redis_node_uri{host="host", port=1234, database=5}, "redis://host:1234/5"},
        {#redis_node_uri{host="host", port=1234}, "redis://host:1234/0"},
        {#redis_node_uri{}, "redis://127.0.0.1:6379/0"},
        {#redis_node_uri{host="[fe80::1]", port=1234, database=5}, "redis://[fe80::1]:1234/5"},
        {#redis_node_uri{host="[fe80::1]"}, "redis://[fe80::1]:6379/0"}
    ],
    [{lists:flatten(io_lib:format("~p", [Rt])),
        fun() -> Rs = to_string(Rt) end} || {Rt, Rs} <- Tests].

-endif.
