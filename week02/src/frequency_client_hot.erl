%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2017 2:42 PM
%%%-------------------------------------------------------------------
-module(frequency_client_hot).
-author("Rakkatakka").

%% API
-export([
    start/0,
    init/0,
    terminate/0,
    create_client/0
]).

start() ->
    Client = spawn(?MODULE, init, []),
    register(?MODULE, Client).

init() ->
    Clients = create_clients(),
    loop(Clients).

terminate() ->
    Client = ?MODULE,
    Client ! {terminate},
    ok.

loop(Clients) ->
    receive
        {terminate} ->
            lists:foreach(fun (Pid) -> exit(Pid, kill) end, Clients)
    end.

create_clients() ->
    Client = ?MODULE,
    Pid1 = spawn_link(Client, create_client, []),
    Pid2 = spawn_link(Client, create_client, []),
    Pid3 = spawn_link(Client, create_client, []),
    Pid4 = spawn_link(Client, create_client, []),
    Pid5 = spawn_link(Client, create_client, []),
    Pid6 = spawn_link(Client, create_client, []),
    [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6].

create_client() ->
    Reply = frequency_server_hot:allocate(),
    io:format("~s - ~p: client received frequency ~p~n", [get_current_time(), self(), Reply]),
    timer:sleep(infinity).

get_current_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).


