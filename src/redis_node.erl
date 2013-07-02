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
-export([start_link/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    {ok, Meta} = setup_redgrid_env(),
    redgrid:start_link(Meta).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

setup_redgrid_env() ->
    Domain = get_env(domain, ?DOMAIN),
    LocalIP = get_env(local_ip, ?LOCAL_IP),
    Version = get_env(version, ?VERSION),
    URI = get_env(uri, ?URI),
    Meta = get_env(meta, ?META),
    application:set_env(redgrid, domain, Domain),
    application:set_env(redgrid, local_ip, LocalIP),
    application:set_env(redgrid, version, Version),
    application:set_env(redgrid, redis_url, URI),
    {ok, Meta}.
