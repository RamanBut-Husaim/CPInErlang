%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2017 12:42 AM
%%%-------------------------------------------------------------------
-module(w_with_mailboxes).
-author("Rakkatakka").

%% API
-export([receiver/0, receiver_case/0]).

receiver() ->
    timer:sleep(1000),
    receive
        stop ->
            io:format("the receiver has been stopped ~n");
        Msg ->
            io:format("the message is ~w ~n", [Msg]),
            receiver()
    end.

receiver_case()->
    timer:sleep(1000),
    receive
        Msg ->
            case Msg of
                stop ->
                    io:format("the receiver has been stopped ~n");
                _ ->
                    io:format("the message is ~w ~n", [Msg]),
                    receiver_case()
            end
    end.
