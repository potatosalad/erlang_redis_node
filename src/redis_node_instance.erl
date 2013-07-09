%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_instance).
-behaviour(gen_server).

-define(PING_TIMEOUT, 1000).
-define(RECONNECT_SLEEP, 1000).

%% API
-export([start_link/8, call/4, cast/2, fsm_call/2, message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    group   = undefined :: undefined | any(),
    channel = undefined :: undefined | binary(),
    handler = undefined :: undefined | module(),
    h_state = undefined :: undefined | any(),
    monitor = undefined :: undefined | reference(),
    pid     = undefined :: undefined | pid(),
    waiting = []        :: [proplists:property()]
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Group, Channel, Host, Port, Database, Password, Handler, HandlerOpts) ->
    gen_server:start_link(?MODULE, [Group, Channel, Host, Port, Database, Password, Handler, HandlerOpts], []).

call(Group, Request, NbNodes, Timeout) ->
    Pid = redis_node_server:get_pid(Group),
    gen_server:call(Pid, {call, Group, Request, NbNodes, Timeout}, infinity).

cast(Group, Request) ->
    Pid = redis_node_server:get_pid(Group),
    gen_server:call(Pid, {cast, Request}).

fsm_call(Group, Request) ->
    Pid = redis_node_server:get_pid(Group),
    Ref = erlang:make_ref(),
    gen_server:call(Pid, {fsm_call, Request, {self(), Ref}}).

message(Group, Message) ->
    Pid = redis_node_server:get_pid(Group),
    gen_server:cast(Pid, {message, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Group, Channel, Host, Port, Database, Password, Handler, HandlerOpts]) ->
    case catch Handler:redis_node_init({Group, Channel}, HandlerOpts) of
        {ok, HandlerState} ->
            case eredis:start_link(Host, Port, Database, Password, ?RECONNECT_SLEEP) of
                {ok, Pid} ->
                    Monitor = erlang:monitor(process, Pid),
                    ok = redis_node_server:set_pid(Group, self()),
                    State = #state{group=Group, channel=Channel, handler=Handler,
                        h_state=HandlerState, monitor=Monitor, pid=Pid},
                    {ok, State};
                RedisError ->
                    {stop, RedisError}
            end;
        HandlerError ->
            {stop, HandlerError}
    end.

handle_call({call, Group, Request, NbNodes, Timeout}, From, State) ->
    supervisor:start_child(redis_node_call_fsm_sup, [Group, From, Request, NbNodes, Timeout]),
    {noreply, State};
handle_call({cast, Request}, _From, State) ->
    publish_cast(Request, State);
handle_call({fsm_call, Request, From}, _From, State) ->
    publish_call(Request, From, State);
handle_call(_Request, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast({message, {'$call', ping, From}}, State) ->
    redis_node:reply(From, pong),
    {noreply, State};
handle_cast({message, {'$call', Request, From}}, State=#state{handler=Handler, h_state=HandlerState}) ->
    case catch Handler:redis_node_call(Request, From, HandlerState) of
        {reply, Reply, HandlerState2} ->
            redis_node:reply(From, Reply),
            {noreply, State#state{h_state=HandlerState2}};
        {reply, Reply, HandlerState2, Timeout} ->
            redis_node:reply(From, Reply),
            {noreply, State#state{h_state=HandlerState2}, Timeout};
        {noreply, HandlerState2} ->
            {noreply, State#state{h_state=HandlerState2}};
        {noreply, HandlerState2, Timeout} ->
            {noreply, State#state{h_state=HandlerState2}, Timeout};
        {stop, Reason, Reply, HandlerState2} ->
            redis_node:reply(From, Reply),
            {stop, Reason, State#state{h_state=HandlerState2}};
        Other ->
            Other
    end;
handle_cast({message, {'$cast', Request}}, State=#state{handler=Handler, h_state=HandlerState}) ->
    case catch Handler:redis_node_cast(Request, HandlerState) of
        {noreply, HandlerState2} ->
            {noreply, State#state{h_state=HandlerState2}};
        {noreply, HandlerState2, Timeout} ->
            {noreply, State#state{h_state=HandlerState2}, Timeout};
        {stop, Reason, HandlerState2} ->
            {stop, Reason, State#state{h_state=HandlerState2}};
        Other ->
            Other
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'$redis_node_reply', {Node, Ref}, Reply}, State=#state{waiting=Waiting}) ->
    case lists:keytake(Ref, 1, Waiting) of
        {value, {Ref, From, _Monitor}, _Waiting2} ->
            catch From ! {'$reply', Node, Reply},
            {noreply, State};
        false ->
            {noreply, State}
    end;
handle_info({'DOWN', Monitor, process, Pid, _Reason}, State=#state{monitor=Monitor, pid=Pid}) ->
    timer:sleep(?RECONNECT_SLEEP),
    {stop, normal, State};
handle_info(Info={'DOWN', Monitor, process, _From, _Reason}, State=#state{waiting=Waiting, handler=Handler, h_state=HandlerState}) ->
    case lists:keytake(Monitor, 3, Waiting) of
        {value, _, Waiting2} ->
            {noreply, State#state{waiting=Waiting2}};
        false ->
            case catch Handler:redis_node_info(Info, HandlerState) of
                {noreply, HandlerState2} ->
                    {noreply, State#state{h_state=HandlerState2}};
                {noreply, HandlerState2, Timeout} ->
                    {noreply, State#state{h_state=HandlerState2}, Timeout};
                {stop, Reason, HandlerState2} ->
                    {stop, Reason, State#state{h_state=HandlerState2}};
                Other ->
                    Other
            end
    end;
handle_info(Info, State=#state{handler=Handler, h_state=HandlerState}) ->
    case catch Handler:redis_node_info(Info, HandlerState) of
        {noreply, HandlerState2} ->
            {noreply, State#state{h_state=HandlerState2}};
        {noreply, HandlerState2, Timeout} ->
            {noreply, State#state{h_state=HandlerState2}, Timeout};
        {stop, Reason, HandlerState2} ->
            {stop, Reason, State#state{h_state=HandlerState2}};
        Other ->
            Other
    end.

terminate(Reason, #state{handler=Handler, h_state=HandlerState}) ->
    Handler:redis_node_terminate(Reason, HandlerState).

code_change(OldVsn, State=#state{handler=Handler, h_state=HandlerState}, Extra) ->
    case catch Handler:redis_node_code_change(OldVsn, HandlerState, Extra) of
        {ok, HandlerState2} ->
            {ok, State#state{h_state=HandlerState2}};
        Error ->
            {error, Error}
    end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

publish_call(Request, {From, Ref}, State=#state{channel=Channel, pid=Pid, waiting=Waiting}) ->
    Call = {'$call', Request, {self(), Ref}},
    case eredis:q(Pid, ["PUBLISH", Channel, term_to_binary(Call)]) of
        {ok, N} ->
            Monitor = erlang:monitor(process, From),
            {reply, {ok, binary_to_integer(N)}, State#state{waiting=[{Ref, From, Monitor} | Waiting]}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

publish_cast(Request, State=#state{channel=Channel, pid=Pid}) ->
    Cast = {'$cast', Request},
    case eredis:q(Pid, ["PUBLISH", Channel, term_to_binary(Cast)]) of
        {ok, _} ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
