-module(misc).
-export([my_tuple_to_list/1, my_time_func/1, my_date_string/0]).

my_tuple_to_list(T) when is_tuple(T) ->
    t2l(T, 1, []).

t2l(T, I, L) when I > size(T) ->
    lists:reverse(L);
t2l(T, I, L) ->
    H = element(I, T),
    t2l(T, I+1, [H|L]).

my_time_func(F) ->
    Before = erlang:now(),
    F(),
    After = erlang:now(),
    MegSec = element(1, After) - element(1, Before),
    Sec = element(2, After) - element(2, Before),
    MySec = element(3, After) - element(3, Before),
    MegSec * 1_000_000 + Sec + MySec / 1_000_000.

my_date_string() ->
    Date = erlang:date(),
    Time = erlang:time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [element(I, Date) || I <- [1,2,3]] ++
                  [element(I, Time) || I <- [1,2,3]]).
