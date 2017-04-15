%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2017 1:38 PM
%%%-------------------------------------------------------------------
-module(sup_for_servers).
-author("Rakkatakka").

-include("sup_frequency.hrl").

%% API
-export([start/0, init/0]).

-type servers() :: #{pid() => atom()}.

start() ->
    Supervisor = ?MODULE,
    Pid = spawn(Supervisor, init, []),
    register(Supervisor, Pid).

init() ->
    process_flag(trap_exit, true),
    ThorPid = create_server(?THOR_SERVER),
    LokiPid = create_server(?LOKI_SERVER),
    loop(#{ ThorPid => ?THOR_SERVER, LokiPid => ?LOKI_SERVER}).

-spec create_server(Name) -> pid() when Name::atom().
create_server(Name) ->
    Pid = spawn_link(sup_frequency_server, start, [self()]),
    register(Name, Pid),
    Pid.

-spec loop(Servers) -> no_return() when Servers:: servers().
loop(Servers) ->
    receive
        {'EXIT', Pid, normal} ->
            case maps:take(Pid, Servers) of
                {Server, NewServers} ->
                    io:format("Frequency Server ~w shut down normally, ignoring~n", [Server]),
                    loop(NewServers);
                _ ->
                    io:format("There is no registered server found ~n"),
                    loop(Servers)
            end;
        {'EXIT', Pid, _Reason} ->
            case maps:take(Pid, Servers) of
                {Server, NewServers} ->
                    io:format("Server ~w died, restarting~n", [Server]),
                    FsPid = create_server(Server),
                    loop(
                        maps:put(FsPid, Server, NewServers)
                    );
                _ ->
                    io:format("There is no registered server found ~n"),
                    loop(Servers)
            end
    end.
