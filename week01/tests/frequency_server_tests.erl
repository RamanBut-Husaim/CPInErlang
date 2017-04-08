%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2017 2:28 PM
%%%-------------------------------------------------------------------
-module(frequency_server_tests).
-author("Rakkatakka").

-include_lib("eunit/include/eunit.hrl").

-import(frequency_server, [
    perform_allocation/2,
    allocate/2,
    perform_deallocation/2,
    deallocate/2
]).

allocate_WhenThereIsNoFrequency_ShouldReturnError_test() ->
    ServerState = {[], #{}},
    Expected = {ServerState, {error, no_frequency}},
    ?assertEqual(Expected, allocate(ServerState, c:pid(3, 10, 1))).

allocate_WhenThereIsAvailableFrequency_ShouldReturnCorrectResult_test() ->
    %% arrange
    ServerState = {[12, 14], #{}},
    Pid = c:pid(3, 10 ,12),

    ExpectedState = {[14], #{Pid => 12}},
    Expected = {ExpectedState, {ok, 12}},

    %% act
    Actual = allocate(ServerState, Pid),

    %% assert
    ?assertEqual(Expected, Actual).

perform_allocation_WhenTheProcessHasAlreadyAllocated_ShouldReturnUser_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ServerState = {[], #{Pid => 33}},

    Expected = {ServerState, {use, 33}},

    %% act
    Actual = perform_allocation(ServerState, Pid),

    %% assert
    ?assertEqual(Expected, Actual).

perform_allocation_WhenThereIsNoAllocationForTheProcess_ShouldAllocateCorrectly_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ExistingPid = c:pid(3, 4, 2),
    ServerState = {[15, 12], #{ ExistingPid => 71}},

    ExpectedState = {[12], #{ ExistingPid => 71, Pid => 15}},
    Expected = {ExpectedState, {ok, 15}},

    %% act
    Actual = perform_allocation(ServerState, Pid),

    %% assert
    ?assertEqual(Expected, Actual).

deallocate_WhenTheFrequencyDoesNotMatchTheAllocatedOne_ShouldReturnForbidden_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ServerState = {[15, 12], #{ Pid => 71}},
    Freq = 33,

    Expected = {ServerState, {error, forbidden}},

    %% act
    Actual = deallocate(ServerState, {Pid, Freq}),

    %% assert
    ?assertEqual(Expected, Actual).

deallocate_WhenTheFrequencyDoesMatchTheAllocatedOne_ShouldReturnCorrectResult_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ServerState = {[15, 12], #{ Pid => 71}},

    ExpectedState = {[71, 15, 12], #{}},
    Expected = {ExpectedState, {ok, 71}},

    %% act
    Actual = deallocate(ServerState, {Pid, 71}),

    %% assert
    ?assertEqual(Expected, Actual).

perform_deallocation_WhenThereIsNoAllocatedFrequency_ShouldReturnError_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ExistingPid = c:pid(3, 4, 2),
    ServerState = {[15, 12], #{ ExistingPid => 71}},

    Expected = {ServerState, {error, no_frequency}},

    %% act
    Actual = perform_deallocation(ServerState, {Pid, 71}),

    %% assert
    ?assertEqual(Expected, Actual).

perform_deallocation_WhenThereIsAllocatedFrequency_ShouldDeallocateCorrectly_test() ->
    %% arrange
    Pid = c:pid(3, 10 ,12),
    ServerState = {[15, 12], #{ Pid => 71}},

    ExpectedState = {[71, 15, 12], #{}},
    Expected = {ExpectedState, {ok, 71}},

    %% act
    Actual = perform_deallocation(ServerState, {Pid, 71}),

    %% assert
    ?assertEqual(Expected, Actual).
