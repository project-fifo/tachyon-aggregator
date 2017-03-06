%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_meta).

-behaviour(gen_server).

%% API
-export([start_link/0, get/1]).
-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DELAY, 60).

-record(state, {
          last_update = 0,
          mdata = #{}
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

get(Short) ->
    gen_server:call(?SERVER, {get, Short}).

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

handle_call({get, Short}, _From, State = #state{mdata = M}) ->
    case maps:find(Short, M) of
        {ok, Meta} ->
            Reply = {ok, Meta},
            {reply, Reply, State};
        error ->
            case maybe_update_map(State) of
                {State1, {ok, M1}} ->
                    State2 = State1#state{
                               mdata = M1
                              },
                    Reply = maps:find(Short, M1),
                    {reply, Reply, State2};
                {State1, Error} ->
                    {reply, Error, State1}
            end
    end.

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

maybe_update_map(S = #state{last_update = T}) ->
    case erlang:system_time(seconds) of
        T1 when (T1 - T) > ?DELAY ->
            {S#state{last_update = T1}, update_meta()};
        _ ->
            {S, error}
    end.

update_meta() ->
    case ls_vm:list([], true) of
        {ok, Es} ->
            Es1 = [map_vm(O) || {_, O} <- Es],
            Es2 = maps:from_list(Es1),
            {ok, Es2};
        E ->
            lager:warning("Metadata update errror: ~p", [E]),
            E
    end.


map_vm(V) ->
    UUID = <<Short:30/binary, _/binary>> = ft_vm:uuid(V),
    O = [
         {<<"fifo">>, <<"owner">>, ft_vm:owner(V)},
         {<<"fifo">>, <<"uuid">>, UUID},
         {<<"fifo">>, <<"hypervisor">>, ft_vm:hypervisor(V)},
         {<<"fifo">>, <<"package">>, ft_vm:package(V)}
        ],
    {Short, O}.
