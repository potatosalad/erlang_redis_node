%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node).

-include("redis_node.hrl").

%% API
-export([start/0, start_group/2, start_group/4, stop_group/1, child_spec/4]).
-export([call/2, call/3, call/4, cast/2, reply/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
    application:start(sasl),
    application:start(redis_node).

start_group(Group, RedisOpts) ->
    start_group(Group, RedisOpts, redis_node_handler, []).

start_group(Group, RedisOpts, Handler, HandlerOpts) ->
    supervisor:start_child(redis_node_sup, child_spec(Group, RedisOpts, Handler, HandlerOpts)).

stop_group(Group) ->
    case supervisor:terminate_child(redis_node_sup, {redis_node_group_sup, Group}) of
        ok ->
            _ = supervisor:delete_child(redis_node_sup, {redis_node_group_sup, Group}),
            redis_node_server:cleanup_group(Group);
        {error, Reason} ->
            {error, Reason}
    end.

child_spec(Group, RedisOpts, Handler, HandlerOpts) ->
    {{redis_node_group_sup, Group},
        {redis_node_group_sup, start_link, [Group, RedisOpts, Handler, HandlerOpts]},
        permanent, 5000, supervisor, [redis_node_group_sup]}.

call(Group, Request) ->
    call(Group, Request, infinity, 1000).

call(Group, Request, NbNodes) ->
    call(Group, Request, NbNodes, 1000).

call(Group, Request, NbNodes, Timeout) ->
    redis_node_instance:call(Group, Request, NbNodes, Timeout).

cast(Group, Request) ->
    redis_node_instance:cast(Group, Request).

reply({To, Tag}, Reply) ->
    catch To ! {'$redis_node_reply', {node(), Tag}, Reply}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
