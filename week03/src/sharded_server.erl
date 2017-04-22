%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2017 12:08 AM
%%%-------------------------------------------------------------------
-module(sharded_server).
-author("Rakkatakka").

%% API
-export([
    start/0,
    init/0
]).

-type freq() :: pos_integer().

start() ->
    Server = ?MODULE,
    Pid = spawn(Server, init, []),
    register(Server, Pid).

init() ->
    ServerEntries = spawn_servers(),
    loop({ServerEntries, 0}).

spawn_servers() ->
    ServerDescriptors = get_server_descriptors(),
    spawn_servers(ServerDescriptors).
spawn_servers(ServerDescriptors) when is_list(ServerDescriptors) ->
    lists:map(fun ({Frequencies, FreqFn}) -> {frequency_server:start(Frequencies), FreqFn} end, ServerDescriptors).

get_server_descriptors() ->
    [
        {get_frequencies_for_first_server(), fun (Freq) -> Freq =< 15 end},
        {get_frequencies_for_second_server(), fun (Freq) -> Freq > 15 end}
    ].

-spec get_frequencies_for_first_server() -> [I, ...] when
    I::freq().
get_frequencies_for_first_server() ->
    [3, 12, 4, 8, 9, 2, 5].

-spec get_frequencies_for_second_server() -> [I, ...] when
    I::freq().
get_frequencies_for_second_server() ->
    [22, 20, 31, 45, 24, 33].

loop({ServerEntries, RoundRobin} = ServerState) ->
    receive
        {request, Pid, allocate} ->
            {ServerPid, _} = lists:nth(RoundRobin + 1, ServerEntries),
            frequency_server:allocate(ServerPid, Pid),

            NewRoundRobin = (RoundRobin + 1) rem length(ServerEntries),
            loop({ServerEntries, NewRoundRobin});
        {request, Pid, {deallocate, Freq}} ->
            {ServerPid, _} = lists:last(
                lists:filter(fun ({_, FreqSelector}) -> FreqSelector(Freq) end, ServerEntries)
            ),
            frequency_server:deallocate(ServerPid, Pid, Freq),

            loop(ServerState);
        {request, _Pid, stop} ->
            lists:foreach(fun ({ServerPid, _}) -> exit(ServerPid, kill) end, ServerEntries)
    end.
