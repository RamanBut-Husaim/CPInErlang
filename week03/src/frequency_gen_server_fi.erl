%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2017 4:34 PM
%%%-------------------------------------------------------------------
-module(frequency_gen_server_fi).
-author("Rakkatakka").

%% API
-export([
    start/0,
    allocate/0,
    deallocate/1,
    report/0,
    stop/0
]).

start() ->
    Freqs = get_frequencies(),
    gen_server:start_link({local, ?MODULE}, frequency_gen_server, Freqs,[]).


allocate() ->
    Res = gen_server:call(?MODULE, {request, allocate}),
    io:format("Allocation result: ~w~n", [Res]).

deallocate(Freq) ->
    Res = gen_server:call(?MODULE, {request, {deallocate, Freq}}),
    io:format("Deallocation result: ~w~n", [Res]).

report() ->
    {Free,Allocated} = gen_server:call(?MODULE, {request, report}),
    io:format("Frequencies available: ~w~n", [Free]),
    io:format("Frequencies allocated: ~w~n", [Allocated]).

stop() ->
    gen_server:cast(?MODULE, {request, stop}),
    ok.

% Hard Coded
-spec(get_frequencies() -> [I, ...] when I::pos_integer()).
get_frequencies() -> [10,11,12,13,14,15].
