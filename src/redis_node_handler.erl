%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_node_handler).

-callback redis_node_init({Group :: any(), Channel :: binary()}, Opts :: any()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback redis_node_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback redis_node_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback redis_node_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback redis_node_terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                               State :: term()) ->
    term().
-callback redis_node_code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                                 Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


%% redis_node_handler callbacks
-export([redis_node_init/2, redis_node_call/3, redis_node_cast/2,
         redis_node_info/2, redis_node_terminate/2, redis_node_code_change/3]).

-record(state, {}).

%%%===================================================================
%%% redis_node_handler callbacks
%%%===================================================================

redis_node_init({_Group, _Channel}, _Opts) ->
    {ok, #state{}}.

redis_node_call(_Request, _From, State) ->
    {reply, {error, undef}, State}.

redis_node_cast(_Request, State) ->
    {noreply, State}.

redis_node_info(_Info, State) ->
    {noreply, State}.

redis_node_terminate(_Reason, _State) ->
    ok.

redis_node_code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
