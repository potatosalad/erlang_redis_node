%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% API
-export([start_link/0, cleanup_group/1, get_pid/1, set_pid/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cleanup_group(Group) ->
    _ = ets:delete(?TAB, {pid, Group}),
    ok.

get_pid(Group) ->
    ets:lookup_element(?TAB, {pid, Group}, 2).

set_pid(Group, Pid) ->
    true = gen_server:call(?SERVER, {set_pid, Group, Pid}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({set_pid, Group, Pid}, _From, State) ->
    case ets:insert_new(?TAB, {{pid, Group}, Pid}) of
        true ->
            {reply, true, State};
        false ->
            {reply, false, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
