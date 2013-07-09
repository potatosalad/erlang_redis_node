%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_call_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/5]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([execute/2, wait/2]).

-record(state, {
    group   = undefined :: undefined | any(),
    from    = undefined :: undefined | {pid(), any()},
    request = undefined :: undefined | any(),
    n_nodes = infinity  :: infinity | non_neg_integer(),
    timeout = 1000      :: infinity | timeout(),
    ref     = undefined :: reference(),
    tref    = undefined :: reference(),
    start   = undefined :: integer(),
    n_reply = 0         :: integer(),
    replies = []        :: list(any())
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Group, From, Request, NbNodes, Timeout) ->
    gen_fsm:start_link(?MODULE, [Group, From, Request, NbNodes, Timeout], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([_Group, From, _Request, infinity, infinity]) ->
    gen_server:reply(From, {error, infinite_loop}),
    {stop, normal};
init([Group, From, Request, NbNodes, Timeout]) ->
    State = #state{group=Group, from=From, request=Request,
        n_nodes=NbNodes, timeout=Timeout, ref=erlang:make_ref()},
    {ok, execute, State, 0}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, 0}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData, 0}.

handle_info('$timeout', _StateName, State=#state{}) ->
    do_reply(State);
handle_info({'$reply', Node, Reply}, StateName, State=#state{n_nodes=NbNodes,
        n_reply=NbReply, replies=Replies, start=Start}) ->
    NbReply2 = NbReply + 1,
    Replies2 = [{Node, timer:now_diff(erlang:now(), Start), Reply} | Replies],
    State2 = State#state{n_reply=NbReply2, replies=Replies2},
    case NbReply2 of
        NbNodes ->
            do_reply(State2);
        _ ->
            {next_state, StateName, State2}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State, 0}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

execute(timeout, State=#state{group=Group, request=Request}) ->
    Start = erlang:now(),
    case catch redis_node_instance:fsm_call(Group, Request) of
        {ok, N} ->
            {ok, State2} = max_nodes(N, State#state{start=Start}),
            case State2#state.timeout of
                infinity ->
                    {next_state, wait, State2};
                Timeout ->
                    TimerRef = erlang:send_after(Timeout, self(), '$timeout'),
                    {next_state, wait, State2#state{tref=TimerRef}}
            end;
        {error, Reason} ->
            gen_server:reply(State#state.from, {error, Reason}),
            {stop, normal, State}
    end.

wait(timeout, State) ->
    {next_state, wait, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

do_reply(State=#state{from=From, replies=Replies}) ->
    gen_server:reply(From, {ok, lists:reverse(Replies)}),
    {stop, normal, State}.

max_nodes(N, State=#state{n_nodes=infinity}) ->
    {ok, State#state{n_nodes=N}};
max_nodes(_N, State) ->
    {ok, State}.
