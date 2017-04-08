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
-export([receiver/0, receiver_case/0, receiver_priority/0]).

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

receiver_priority() ->
    Threshold = 5,
    receive_messages(Threshold, []).

receive_messages(Threshold, MessagePool) when length(MessagePool) =:= Threshold ->
    display_message_pool_in_order(MessagePool),
    receive_messages(Threshold, []);
receive_messages(Threshold, MessagePool) ->
    receive
        stop ->
            display_message_pool_in_order(MessagePool),
            io:format("the receiver has been stopped ~n");
        {first, Msg} ->
            receive_messages(Threshold, [{Msg, 1} | MessagePool]);
        {second, Msg} ->
            receive_messages(Threshold, [{Msg, 2} | MessagePool])
    end.

display_message_pool_in_order(MessagePool) ->
    Messages = order_messages(MessagePool),
    display_messages(Messages).

order_messages(MessagePool) ->
    lists:map(fun ({Msg, _}) -> Msg end,
        lists:sort(fun ({_, T1}, {_, T2}) -> T1 =< T2 end,
            lists:reverse(MessagePool)
        )
    ).

display_messages(MessagePool) ->
    lists:foreach(
        fun (Msg) -> io:format("~s~n", [Msg]) end,
        MessagePool).
