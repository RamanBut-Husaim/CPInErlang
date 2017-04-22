%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2017 2:01 PM
%%%-------------------------------------------------------------------
-module(frequency_server).
-author("Rakkatakka").

%% API
-export([
    start/1,
    init/1,
    allocate/2,
    deallocate/3,
    inject/3,
    stop/2,
    loop/1
]).

%% These are the start functions used to create and
%% initialize the server.

start(Frequencies) ->
    Server = ?MODULE,
    spawn(Server, init,[Frequencies]).

init(FrequencyList) ->
    process_flag(trap_exit, true),
    Frequencies = {FrequencyList, #{}},
    loop(Frequencies).

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
-type error_timeout() :: {error, timeout}.

-type allocate_operation_result() :: ok_freq() | error_no_freq() | use_freq().
-type deallocate_operation_result() :: ok_freq() | error_no_freq() | error_forbidden().
-type injection_operation_result() :: {ok}.
-type operation_result() :: {server_state(), allocate_operation_result() | deallocate_operation_result() | injection_operation_result()}.
-type server_reply() :: allocate_operation_result() | deallocate_operation_result() | injection_operation_result() | stopped.
-type server_result() :: {reply, server_reply()}.

%% Functional interface

-spec allocate(ServerPid, ClientPid) -> allocate_operation_result() | error_timeout() when
    ServerPid::pid(),
    ClientPid::pid().
allocate(ServerPid, ClientPid) ->
    send_client_message(ServerPid, {request, ClientPid, allocate}).

-spec deallocate(ServerPid, ClientPid, Freq) -> deallocate_operation_result() | error_timeout() when
    ServerPid::pid(),
    ClientPid::pid(),
    Freq::freq().
deallocate(ServerPid, ClientPid, Freq) ->
    send_client_message(ServerPid, {request, ClientPid, {deallocate, Freq}}).

-spec inject(ServerPid, ClientPid, Frequencies) -> injection_operation_result() | error_timeout() when
    ServerPid::pid(),
    ClientPid::pid(),
    Frequencies::freq_list().
inject(ServerPid, ClientPid, Frequencies) ->
    send_client_message(ServerPid, {request, ClientPid, {inject, Frequencies}}).

-spec stop(ServerPid, ClientPid) -> stopped when
    ServerPid::pid(),
    ClientPid::pid().
stop(ServerPid, ClientPid) ->
    send_client_message(ServerPid, {request, ClientPid, stop}).

send_client_message(ServerPid, Msg) ->
    clear(),
    ServerPid ! Msg,
    receive_client_message().

receive_client_message() ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~s - ~p: server process ~p died with reason ~p ~n", [get_current_time(), self(), Pid, Reason]),
            {error, server_died};

        {reply, Reply} ->
            Reply
    after 1000 ->
        timeout()
    end.

-spec clear() -> ok.
clear() ->
    receive
        Msg ->
            io:format("cleared messsage: ~w~n", [Msg]),
            clear()
    after 0 ->
        ok
    end.

-spec loop(State) -> server_result() when State::server_state().
loop(State) ->
    receive
        {request, Pid, {inject, NewFrequencies}} ->
            {NewState, Reply} = perform_injection(State, NewFrequencies),
            Pid ! {reply, Reply},
            ?MODULE:loop(NewState);
        {request, Pid, allocate} ->
            {NewState, Reply} = perform_allocation(State, Pid),
            Pid ! {reply, Reply},
            ?MODULE:loop(NewState);
        {request, Pid , {deallocate, Freq}} ->
            {NewState, Reply} = perform_deallocation(State, {Pid, Freq}),
            Pid ! {reply, Reply},
            ?MODULE:loop(NewState);
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', Pid, _Reason} ->
            NewState = exited(State, Pid),
            ?MODULE:loop(NewState)
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

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
    link(Pid),
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
            unlink(Pid),
            create_deallocation_result({[Freq | Free], maps:remove(Pid, Allocations)}, ok(Freq));
        _ ->
            create_deallocation_result(State, forbidden())
    end.

-spec exited(ServerState, Pid) -> server_state() when
    ServerState::server_state(),
    Pid::pid().
exited({Free, Allocations} = State, Pid) ->
    case maps:get(Pid, Allocations) of
        Freq ->
            {[Freq | Free], maps:remove(Pid, Allocations)};
        _ -> State
    end.

get_current_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

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

-spec timeout() -> error_timeout().
timeout() ->
    {error, timeout}.
