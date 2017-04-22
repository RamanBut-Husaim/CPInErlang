%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2017 4:06 PM
%%%-------------------------------------------------------------------
-module(frequency_gen_server).
-author("Rakkatakka").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% types

%% domain
-type freq() :: pos_integer().
-type allocations() :: #{pid() => freq()}.
-type freq_list() :: [freq()].
-type server_state() :: {freq_list(), allocations()}.

%% results

-type ok_freq() :: {ok, freq()}.
-type error_no_freq() :: {error, no_frequency}.
-type use_freq() :: {use, freq()}.
-type error_forbidden() :: {error, forbidden}.

-type allocate_operation_result() :: ok_freq() | error_no_freq() | use_freq().
-type deallocate_operation_result() :: ok_freq() | error_no_freq() | error_forbidden().
-type injection_operation_result() :: {ok}.
-type operation_result() :: {server_state(), allocate_operation_result() | deallocate_operation_result() | injection_operation_result()}.

%%%===================================================================
%%% API
%%%===================================================================

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Frequencies) ->
    State = {Frequencies, #{}},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({request, allocate}, {From, _Ref}, State) ->
    {NewState, Reply} = perform_allocation(State, From),
    {reply, Reply, NewState};

handle_call({request, {deallocate, Freq}}, {From, _Ref}, State) ->
    {NewState, Reply} = perform_deallocation(State, {From, Freq}),
    {reply, Reply, NewState};

handle_call({request, {inject, NewFrequencies}}, _From, State) ->
    {NewState, Reply} = perform_injection(State, NewFrequencies),
    {reply, Reply, NewState};

handle_call({request, report}, _From, {Frequencies, Allocations} = State) ->
    Reply = {length(Frequencies), maps:size(Allocations)},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({request, stop}, State) ->
    {stop, stopped, State}.

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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec perform_allocation(ServerState, Pid) -> operation_result() when
    ServerState::server_state(),
    Pid::pid().
perform_allocation({_, Allocations} = ServerState, Pid) ->
    case maps:is_key(Pid, Allocations) of
        true ->
            Freq = maps:get(Pid, Allocations),
            create_allocation_result(ServerState, use(Freq));
        _ ->
            allocate_internal(ServerState, Pid)
    end.

-spec create_allocation_result(State, OpResult) -> operation_result() when
    State::server_state(),
    OpResult::allocate_operation_result().
create_allocation_result(ServerState, AllocateOperationResult) ->
    {ServerState, AllocateOperationResult}.

-spec allocate_internal(ServerState, Pid) -> operation_result() when
    ServerState::server_state(),
    Pid::pid().
allocate_internal({[], _} = ServerState, _Pid) ->
    create_allocation_result(ServerState, no_frequency());
allocate_internal({[Freq|Free], Allocations}, Pid) ->
    create_allocation_result({Free, maps:put(Pid, Freq, Allocations)}, ok(Freq)).

-spec perform_injection(ServerState, NewFrequencies) -> operation_result() when
    ServerState::server_state(),
    NewFrequencies::freq_list().
perform_injection(ServerState, NewFrequencies) when is_list(NewFrequencies) ->
    inject_internal(ServerState, NewFrequencies).

-spec inject_internal(ServerState, NewFrequencies) -> operation_result() when
    ServerState::server_state(),
    NewFrequencies::freq_list().
inject_internal({FreqList, Allocations}, NewFrequencies) ->
    ConcatenatedList = lists:append(FreqList, NewFrequencies),
    create_injection_result({ConcatenatedList, Allocations}).

create_injection_result(ServerState) ->
    {ServerState, ok()}.

-spec perform_deallocation(ServerState, Request) -> operation_result() when
    ServerState::server_state(),
    Request::{pid(), freq()}.
perform_deallocation({_, Allocations} = ServerState, {Pid, _} = Request) ->
    case maps:is_key(Pid, Allocations) of
        true -> deallocate_internal(ServerState, Request);
        _ -> create_deallocation_result(ServerState, no_frequency())
    end.

-spec create_deallocation_result(State, OpResult) -> operation_result() when
    State::server_state(),
    OpResult::deallocate_operation_result().
create_deallocation_result(ServerState, DeallocationOperationResult) ->
    {ServerState, DeallocationOperationResult}.

-spec deallocate_internal(ServerState, Request) -> operation_result() when
    ServerState::server_state(),
    Request::{pid(), freq()}.
deallocate_internal({Free, Allocations} = State, {Pid, Freq}) ->
    case maps:get(Pid, Allocations) of
        Freq ->
            create_deallocation_result({[Freq | Free], maps:remove(Pid, Allocations)}, ok(Freq));
        _ ->
            create_deallocation_result(State, forbidden())
    end.

-spec use(Freq) -> use_freq() when
    Freq::freq().
use(Freq) ->
    {use, Freq}.

-spec forbidden() -> error_forbidden().
forbidden() ->
    {error, forbidden}.

-spec ok(Freq) -> ok_freq() when
    Freq::freq().
ok(Freq) ->
    {ok, Freq}.

-spec ok() -> {ok}.
ok() ->
    {ok}.

-spec no_frequency() -> error_no_freq().
no_frequency() ->
    {error, no_frequency}.
