%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_idx).

-behaviour(gen_server).

%% API
-export([start_link/0, put/5]).

-ignore_xref([start_link/0]).


%% CAlled from the compiled module
-ignore_xref([put/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(FALSE_POSITIVES, 0.001).
-define(INITIAL_SIZE, 1000000).
-define(MAX_LEN, 10).

-define(SERVER, ?MODULE).

-record(state, {bloom = bloom:sbf(?INITIAL_SIZE)}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put(Collection, Metric, Bucket, Key, Dimensions) ->
    put(Collection, Metric, Bucket, Key, Dimensions, ?MAX_LEN).

put(Collection, Metric, Bucket, Key, Dimensions, MaxLen) ->
    case erlang:process_info(whereis(?SERVER), message_queue_len) of
        {message_queue_len, N} when N > MaxLen ->
            ok;
        _ ->
            gen_server:cast(
              ?SERVER, {put, Collection, Metric, Bucket, Key, Dimensions})
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({put, _Collection, _Metric, Bucket, Key, _Dimensions},
            State) ->
    case known(Bucket, Key, State) of
        {true, State1} ->
            {noreply, State1};
        {false, State1} ->
            io:format("New Metric: ~p", [dproto:metric_to_list(Key)]),
            %%ddb_idx:add(Collection, Metric, Bucket, Key, Dimensions),
            {noreply, State1}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

known(Bucket, Metric, State = #state{bloom = Bloom}) ->
    E = term_to_binary({Bucket, Metric}),
    case bloom:member(E, Bloom) of
        true ->
            {true, State};
        _ ->
            {false, State#state{bloom = bloom:add(E, Bloom)}}
    end.
