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
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), #{}},
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
-type operation_result() :: {server_state(), allocate_operation_result() | deallocate_operation_result()}.
-type server_reply() :: allocate_operation_result() | deallocate_operation_result() | stopped.
-type server_result() :: {reply, server_reply()}.

%% Functional interface

-spec allocate() -> allocate_operation_result() | error_timeout().
allocate() ->
    send_client_message({request, self(), allocate}).

-spec deallocate(Freq) -> deallocate_operation_result() | error_timeout() when Freq::freq().
deallocate(Freq) ->
    send_client_message({request, self(), {deallocate, Freq}}).

-spec stop() -> stopped.
stop() ->
    send_client_message({request, self(), stop}).

send_client_message(Msg) ->
    clear(),
    Server = ?MODULE,
    case whereis(Server) of
        undefined ->
            {error, server_not_found};
        _ ->
            Server ! Msg,
            receive_client_message()
    end.

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

% Hard Coded
-spec get_frequencies() -> [I, ...] when I :: freq().
get_frequencies() -> [10,11,12,13,14,15].

-spec loop(State) -> server_result() when State::server_state().
loop(State) ->
    receive
        {request, Pid, allocate} ->
            {NewState, Reply} = perform_allocation(State, Pid),
            Pid ! {reply, Reply},
            loop(NewState);
        {request, Pid , {deallocate, Freq}} ->
            {NewState, Reply} = perform_deallocation(State, {Pid, Freq}),
            Pid ! {reply, Reply},
            loop(NewState);
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', Pid, _Reason} ->
            NewState = exited(State, Pid),
            loop(NewState)
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
            allocate(ServerState, Pid)
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
    create_allocation_result(ServerState, no_frequency());
allocate({[Freq|Free], Allocations}, Pid) ->
    link(Pid),
    create_allocation_result({Free, maps:put(Pid, Freq, Allocations)}, ok(Freq)).

-spec perform_deallocation(ServerState, Request) -> operation_result() when
    ServerState::server_state(),
    Request::{pid(), freq()}.
perform_deallocation({_, Allocations} = ServerState, {Pid, _} = Request) ->
    case maps:is_key(Pid, Allocations) of
        true -> deallocate(ServerState, Request);
        _ -> create_deallocation_result(ServerState, no_frequency())
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

-spec no_frequency() -> error_no_freq().
no_frequency() ->
    {error, no_frequency}.

-spec timeout() -> error_timeout().
timeout() ->
    {error, timeout}.
