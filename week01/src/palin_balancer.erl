%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2017 6:29 PM
%%%-------------------------------------------------------------------
-module(palin_balancer).
-author("Rakkatakka").

%% API
-export([start/0]).

start() ->
    NumberOfServers = 2,
    Servers = create_server_instances(NumberOfServers),
    process_messages(Servers, 0).

process_messages(Servers, Counter) ->
    NumberOfServers = length(Servers),
    receive
        stop ->
            stop_servers(Servers),
            io:format("the load balancers has been stopped ~n");
        Msg ->
            ServerIndex = Counter rem NumberOfServers,
            Server = lists:nth(ServerIndex + 1, Servers),
            Server ! Msg,
            process_messages(Servers, Counter + 1)
    end.

create_server_instances(NumberOfServers) ->
    create_server_instances(NumberOfServers, []).

create_server_instances(0, Servers) ->
    Servers;
create_server_instances(ServersToCreate, Servers) ->
    Server = spawn(palin_server_m, start, []),
    create_server_instances(ServersToCreate - 1, [Server | Servers]).

stop_servers([]) ->
    ok;
stop_servers([Server | Servers]) ->
    Server ! stop,
    stop_servers(Servers).

