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
-define(MAX_LEN, 1000).
%% 1 hour
-define(MDATA_DELAY, 60*60).

-define(SERVER, ?MODULE).

-record(state, {
          bloom = bloom:sbf(?INITIAL_SIZE),
          mdata_bloom = bloom:sbf(?INITIAL_SIZE),
          last_update = 0
         }).

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
    dqe_idx:init(),
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
handle_cast({put, Collection, Metric, Bucket, Key, Dimensions},
            State) ->
    State1 = update_idx(Collection, Metric, Bucket, Key, Dimensions, State),
    State2 = case lists:keyfind(<<"uuid">>, 2, Dimensions) of
                 {<<"kstat">>, <<"uuid">>, <<"global">>} ->
                     State1;
                 {<<"kstat">>, <<"uuid">>, UUID} ->
                     update_mdata(Collection, Metric, Bucket, Key, UUID,
                                  State1);
                 _ ->
                     State1
    end,
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

update_idx(Collection, Metric, Bucket, Key, Dimensions, State) ->
    case known(Bucket, Key, State) of
        {true, StateK} ->
            StateK;
        {false, StateK} ->
            dqe_idx:add(Collection, Metric, Bucket, Key, Dimensions),
            StateK
    end.

update_mdata(Collection, Metric, Bucket, Key, UUID,
             State = #state{last_update = T}) ->
    case erlang:system_time(seconds) of
        T1 when (T1 - T) > ?MDATA_DELAY ->
            update_mdata_(Collection, Metric, Bucket, Key, UUID,
                          State#state{mdata_bloom = bloom:sbf(?INITIAL_SIZE),
                                      last_update = T1});
        _ ->
            update_mdata_(Collection, Metric, Bucket, Key, UUID, State)
    end.

update_mdata_(Collection, Metric, Bucket, Key, UUID,
             State = #state{mdata_bloom = Bloom}) ->
    E = term_to_binary({Bucket, Key}),
    case bloom:member(E, Bloom) of
        true ->
            State;
        _ ->
            case tachyon_meta:get(UUID) of
                {ok, MData} ->
                    dqe_idx:update(Collection, Metric, Bucket, Key, MData),
                    State#state{mdata_bloom = bloom:add(E, Bloom)};
                _ ->
                    State
            end
    end.


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

known(Bucket, Key, State = #state{bloom = Bloom}) ->
    E = term_to_binary({Bucket, Key}),
    case bloom:member(E, Bloom) of
        true ->
            {true, State};
        _ ->
            {false, State#state{bloom = bloom:add(E, Bloom)}}
    end.
