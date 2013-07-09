%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   6 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_group_sup).
-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Group, RedisOpts, Handler, HandlerOpts) ->
    supervisor:start_link(?MODULE, {Group, RedisOpts, Handler, HandlerOpts}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Group, RedisOpts, Handler, HandlerOpts}) ->
    URI = redis_node_uri:from_string(proplists:get_value(uri, RedisOpts, "")),
    [Host, Port, Database, Password] = redis_node_uri:get([host, port, database, password], URI),
    Version = proplists:get_value(version, RedisOpts, 0),
    Channel = proplists:get_value(channel, RedisOpts, <<"redis_node:", (atom_to_binary(Group, utf8))/binary, ":", (integer_to_binary(Version))/binary>>),
    {ok, {{rest_for_one, 5, 10}, [
        {redis_node_instance,
            {redis_node_instance, start_link, [Group, Channel, Host, Port, Database, Password, Handler, HandlerOpts]},
            permanent, infinity, worker, [redis_node_instance]},
        {redis_node_subscribe,
            {redis_node_subscribe, start_link, [Group, Channel, Host, Port, Password]},
            permanent, infinity, worker, [redis_node_subscribe]}
    ]}}.
