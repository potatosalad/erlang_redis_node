%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_subscribe).
-behaviour(gen_fsm).

-define(RECONNECT_SLEEP, 1000).

%% API
-export([start_link/5, stop/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([connecting/2, subscribing/2, ready/2]).

-record(state, {
    group    = undefined :: undefined | any(),
    channel  = undefined :: undefined | binary(),
    host     = undefined :: undefined | list(),
    port     = undefined :: undefined | integer(),
    password = undefined :: undefined | list(),
    monitor  = undefined :: undefined | reference(),
    pid      = undefined :: undefined | pid()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Group, Channel, Host, Port, Password) ->
    gen_fsm:start_link(?MODULE, [Group, Channel, Host, Port, Password], []).

stop(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Group, Channel, Host, Port, Password]) ->
    ok = redis_node_server:set_pid({subscribe, Group}, self()),
    State = #state{group=Group, channel=Channel, host=Host, port=Port, password=Password},
    {ok, connecting, State, 0}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, 0}.

handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, State};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData, 0}.

handle_info({dropped, _, Pid}, ready, State=#state{pid=Pid}) ->
    ok = eredis_sub:ack_message(Pid),
    {next_state, ready, State};
handle_info({eredis_connected, Pid}, _StateName, State=#state{pid=Pid}) ->
    ok = eredis_sub:ack_message(Pid),
    {next_state, subscribing, State, 0};
handle_info({eredis_disconnected, Pid}, _StateName, State=#state{pid=Pid}) ->
    ok = eredis_sub:ack_message(Pid),
    {next_state, connecting, State, 0};
handle_info({message, Channel, Message, Pid}, ready,
        State=#state{channel=Channel, pid=Pid}) ->
    {ok, State2} = parse_message(Message, State),
    ok = eredis_sub:ack_message(Pid),
    {next_state, ready, State2};
handle_info({subscribed, Channel, Pid}, subscribing,
        State=#state{channel=Channel, pid=Pid}) ->
    ok = eredis_sub:ack_message(Pid),
    {next_state, ready, State};
handle_info({'DOWN', Monitor, process, Pid, _Reason}, _StateName,
        State=#state{monitor=Monitor, pid=Pid}) ->
    {next_state, connecting, State#state{monitor=undefined, pid=undefined}, 0};
handle_info(_Info, StateName, State=#state{pid=Pid}) ->
    io:format("GOT BAD INFO: ~p ~p~n", [StateName, _Info]),
    ok = eredis_sub:ack_message(Pid),
    {next_state, StateName, State, 0}.

terminate(_Reason, _StateName, #state{pid=Pid}) when Pid =/= undefined ->
    catch eredis_sub:stop(Pid),
    ok;
terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

connecting(timeout, State=#state{host=Host, port=Port, password=Password, pid=undefined}) ->
    case catch eredis_sub:start_link(Host, Port, Password, ?RECONNECT_SLEEP, infinity, exit) of
        {ok, Pid} ->
            case catch eredis_sub:controlling_process(Pid) of
                ok ->
                    Monitor = erlang:monitor(process, Pid),
                    {next_state, subscribing, State#state{monitor=Monitor, pid=Pid}, 0};
                _ ->
                    catch eredis_sub:stop(Pid),
                    {next_state, connecting, State, ?RECONNECT_SLEEP}
            end;
        _ ->
            {next_state, connecting, State, ?RECONNECT_SLEEP}
    end;
connecting(timeout, State=#state{pid=Pid}) when Pid =/= undefined ->
    {next_state, connecting, State}.

subscribing(timeout, State=#state{channel=Channel, pid=Pid}) ->
    case catch eredis_sub:subscribe(Pid, [Channel]) of
        ok ->
            {next_state, subscribing, State};
        _ ->
            {next_state, connecting, State, 0}
    end.

ready(_Event, State) ->
    {next_state, ready, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

parse_message(Message, State=#state{group=Group}) ->
    try binary_to_term(Message) of
        Term ->
            ok = redis_node_instance:message(Group, Term),
            {ok, State}
    catch
        Class:Reason ->
            error_logger:error_msg(
                "** ~p non-fatal error in ~p/~p~n"
                "** Unhandled Message: ~p~n** Error: ~p:~p~n~n",
                [?MODULE, parse_message, 2, Message, Class, Reason]),
            {ok, State}
    end.
