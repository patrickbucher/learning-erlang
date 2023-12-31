-module(misc).
-export([my_tuple_to_list/1]).

my_tuple_to_list(T) when is_tuple(T) ->
    t2l(T, 1, []).

t2l(T, I, L) when I > size(T) ->
    lists:reverse(L);
t2l(T, I, L) ->
    H = element(I, T),
    t2l(T, I+1, [H|L]).

