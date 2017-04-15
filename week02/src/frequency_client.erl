%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2017 11:38 AM
%%%-------------------------------------------------------------------
-module(frequency_client).
-author("Rakkatakka").

%% API

-export([start/0, start_client/3]).

start() ->
    Client = ?MODULE,
    spawn(Client, start_client, ["Thor", 1000, 2000]),
    spawn(Client, start_client, ["Odin", 2000, 1500]).

-spec start_client(Id, ProcessingDuration, WaitDuration) -> no_return() when
    Id::string(),
    ProcessingDuration::pos_integer(),
    WaitDuration::pos_integer().
start_client(Id, ProcessingDuration, WaitDuration) ->
    io:format("~s - ~p: client (~p) resting~n", [get_current_time(), self(), Id]),
    process_flag(trap_exit, true),
    client_message_loop(Id, ProcessingDuration, WaitDuration).

-spec client_message_loop(Id, ProcessingDuration, WaitDuration) -> no_return() when
    Id::string(),
    ProcessingDuration::pos_integer(),
    WaitDuration::pos_integer().
client_message_loop(Id, ProcessingDuration, WaitDuration) ->
    io:format("~s - ~p: client (~p) resting~n", [get_current_time(), self(), Id]),
    timer:sleep(WaitDuration),
    io:format("~s - ~p: client (~p) start processing~n", [get_current_time(), self(), Id]),
    {Status, Res} =
        case frequency_server:allocate() of
            {ok, Freq} = OperationResult ->
                io:format("~s - ~p: cleint (~p) received frequency ~p~n", [get_current_time(), self(), Id, Freq]),
                OperationResult;
            {error, Error} = OperationResult ->
                io:format("~s - ~p: client (~p) got error ~p.~n", [get_current_time(), self(), Id, Error]),
                OperationResult
        end,
    timer:sleep(ProcessingDuration),
    io:format("~s - ~p: client ~p ending processing~n", [get_current_time(), self(), Id]),

    case Status of
        ok -> frequency_server:deallocate(Res);
        _ -> io:format("~s - ~p: client ~p skip deallocation~n", [get_current_time(), self(), Id])
    end,

    client_message_loop(Id, ProcessingDuration, WaitDuration).

get_current_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).