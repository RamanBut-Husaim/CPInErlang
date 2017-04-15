%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2017 2:43 PM
%%%-------------------------------------------------------------------
-module(sup_frequency_client).
-author("Rakkatakka").

-include("sup_frequency.hrl").

-export([start/0, start_client/3]).

%% API
start() ->
    Client = ?MODULE,
    spawn(Client, start_client, [?THOR_SERVER, 10000, 2000]),
    spawn(Client, start_client, [?LOKI_SERVER, 20000, 1500]).

-spec start_client(Id, ProcessingDuration, WaitDuration) -> no_return() when
    Id::atom(),
    ProcessingDuration::pos_integer(),
    WaitDuration::pos_integer().
start_client(Id, ProcessingDuration, WaitDuration) ->
    io:format("~s - ~p: client (~p) resting~n", [get_current_time(), self(), Id]),
    client_message_loop(Id, ProcessingDuration, WaitDuration).

-spec client_message_loop(Id, ProcessingDuration, WaitDuration) -> no_return() when
    Id::atom(),
    ProcessingDuration::pos_integer(),
    WaitDuration::pos_integer().
client_message_loop(Id, ProcessingDuration, WaitDuration) ->
    io:format("~s - ~p: client (~p) resting~n", [get_current_time(), self(), Id]),
    timer:sleep(WaitDuration),
    io:format("~s - ~p: client (~p) start processing~n", [get_current_time(), self(), Id]),
    case sup_frequency_server:allocate(Id) of
        {ok, Freq} ->
            io:format("~s - ~p: client (~p) received frequency ~p~n", [get_current_time(), self(), Id, Freq]),
            timer:sleep(ProcessingDuration),
            io:format("~s - ~p: client (~p) ending processing~n", [get_current_time(), self(), Id]),
            sup_frequency_server:deallocate(Id, Freq),
            client_message_loop(Id, ProcessingDuration, WaitDuration);

        {error, Error} ->
            io:format("~s - ~p: client (~p) got error ~p.~n", [get_current_time(), self(), Id, Error])
    end.

get_current_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).
