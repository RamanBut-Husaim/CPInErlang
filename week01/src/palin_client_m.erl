%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2017 6:03 PM
%%%-------------------------------------------------------------------
-module(palin_client_m).
-author("Rakkatakka").

%% API
-export([start/1]).

start(ServerPid) ->
    request_generator(ServerPid, 0).

request_generator(ServerPid, MessageIndex) ->
    Message = {check, self(), "Message"},
    ServerPid ! Message,
    timer:sleep(500),

    receive
        stop ->
            io:format("the client has been stopped ~n");
        {result, ServerMessage} ->
            io:format("the server response is: ~s~n", [ServerMessage]),
            request_generator(ServerPid, MessageIndex + 1)
    end.



