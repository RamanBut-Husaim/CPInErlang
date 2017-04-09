%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2017 12:43 PM
%%%-------------------------------------------------------------------
-module(frequency_server).
-author("Rakkatakka").

%% API
-export([
    start/0,
    init/0,
    allocate/0,
    deallocate/1,
    stop/0,
    perform_allocation/2,
    allocate/2,
    perform_deallocation/2,
    deallocate/2]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    Server = ?MODULE,
    Pid = spawn(Server, init,[]),
    register(Server, Pid).

init() ->
    Frequencies = {get_frequencies(), #{}},
    loop(Frequencies).

%% types

%% domain
-type freq() :: integer().
-type allocations() :: #{pid() => freq()}.
-type freq_list() :: [freq()].
-type server_state() :: {freq_list(), allocations()}.

%% results

-type allocate_operation_result() :: {ok, freq()} | {error, no_frequency} | {use, freq()}.
-type deallocate_operation_result() :: {ok, freq()} | {error, no_frequency} | {error, forbidden}.
-type operation_result() :: {server_state(), allocate_operation_result() | deallocate_operation_result()}.
-type server_reply() :: allocate_operation_result() | deallocate_operation_result() | stopped.
-type server_result() :: {reply, server_reply()}.

%% Functional interface

-spec allocate() -> allocate_operation_result() | {error, timeout}.
allocate() ->
    clear(),
    Server = ?MODULE,
    Server ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply
    after 1000 ->
        {error, timeout}
    end.

-spec deallocate(Freq) -> deallocate_operation_result() | {error, timeout} when Freq::freq().
deallocate(Freq) ->
    clear(),
    Server = ?MODULE,
    Server ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} -> Reply
    after 1000 ->
        {error, timeout}
    end.

-spec stop() -> stopped.
stop() ->
    clear(),
    Server = ?MODULE,
    Server ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
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

% Hard Coded
-spec get_frequencies() -> [I, ...] when I :: freq().
get_frequencies() -> [10,11,12,13,14,15].

-spec loop(State) -> server_result() when State::server_state().
loop(State) ->
    receive
        {request, Pid, allocate} ->
            {NewState, Reply} = perform_allocation(State, Pid),
            timer:sleep(1100),
            Pid ! {reply, Reply},
            loop(NewState);
        {request, Pid , {deallocate, Freq}} ->
            {NewState, Reply} = perform_deallocation(State, {Pid, Freq}),
            timer:sleep(1100),
            Pid ! {reply, Reply},
            loop(NewState);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

-spec perform_allocation(ServerState, Pid) -> operation_result() when
    ServerState::server_state(),
    Pid::pid().
perform_allocation({_, Allocations} = ServerState, Pid) ->
    case maps:is_key(Pid, Allocations) of
        true -> create_allocation_result(ServerState, {use, maps:get(Pid, Allocations)});
        _ -> allocate(ServerState, Pid)
    end.

-spec create_allocation_result(State, OpResult) -> operation_result() when
    State::server_state(),
    OpResult::allocate_operation_result().
create_allocation_result(ServerState, AllocateOperationResult) ->
    {ServerState, AllocateOperationResult}.

-spec allocate(ServerState, Pid) -> operation_result() when
    ServerState::server_state(),
    Pid::pid().
allocate({[], _} = ServerState, _Pid) ->
    create_allocation_result(ServerState, {error, no_frequency});
allocate({[Freq|Free], Allocations}, Pid) ->
    create_allocation_result({Free, maps:put(Pid, Freq, Allocations)}, {ok, Freq}).

-spec perform_deallocation(ServerState, Request) -> operation_result() when
    ServerState::server_state(),
    Request::{pid(), freq()}.
perform_deallocation({_, Allocations} = ServerState, {Pid, _} = Request) ->
    case maps:is_key(Pid, Allocations) of
        true -> deallocate(ServerState, Request);
        _ -> create_deallocation_result(ServerState, {error, no_frequency})
    end.

-spec create_deallocation_result(State, OpResult) -> operation_result() when
    State::server_state(),
    OpResult::deallocate_operation_result().
create_deallocation_result(ServerState, DeallocationOperationResult) ->
    {ServerState, DeallocationOperationResult}.

-spec deallocate(ServerState, Request) -> operation_result() when
    ServerState::server_state(),
    Request::{pid(), freq()}.
deallocate({Free, Allocations} = State, {Pid, Freq}) ->
    case maps:get(Pid, Allocations) of
        Freq -> create_deallocation_result({[Freq | Free], maps:remove(Pid, Allocations)}, {ok, Freq});
        _ -> create_deallocation_result(State, {error, forbidden})
    end.
