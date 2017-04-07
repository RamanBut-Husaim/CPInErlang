%%%-------------------------------------------------------------------
%%% @author Rakkatakka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2017 11:48 PM
%%%-------------------------------------------------------------------
-module(palin_server).
-author("Rakkatakka").

%% API
-export([start/1]).

start(Pid) ->
    receive
        stop ->
            io:format("the server has been stopped ~n");
        {check, Text} ->
            Pid ! {result, build_result_message(Text)},
            start(Pid)
    end.

build_result_message(Text) ->
    IsPalindrome = palindrome_check(Text),
    case IsPalindrome of
        true ->
            string:concat(Text, " is a palindrome.");
        _ ->
            string:concat(Text, " is not a palindrome.")
    end.

palindrome_check(String) ->
    Normalise = to_small(rem_punct(String)),
    lists:reverse(Normalise) == Normalise.

rem_punct(String) -> lists:filter(fun (Ch) ->
    not(lists:member(Ch,"\"\'\t\n "))
                                  end,
    String).

to_small(String) -> lists:map(fun(Ch) ->
    case ($A =< Ch andalso Ch =< $Z) of
        true -> Ch+32;
        false -> Ch
    end
                              end,
    String).
