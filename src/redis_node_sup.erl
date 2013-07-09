%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Config = case application:get_env(redis_node, groups) of
        {ok, C} ->
            C;
        undefined ->
            []
    end,
    ChildSpecs = [begin
        redis_node:child_spec(Group, RedisOpts, Handler, HandlerOpts)
    end || {Group, RedisOpts, Handler, HandlerOpts} <- Config],
    redis_node_server = ets:new(redis_node_server, [ordered_set, public, named_table]),
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(redis_node_server, worker),
        ?CHILD(redis_node_call_fsm_sup, supervisor)
        | ChildSpecs
    ]}}.

