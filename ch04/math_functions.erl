-module(math_functions).
-export([even/1, odd/1, filter/2, split/1, splitacc/1]).

even(X) -> X rem 2 == 0.
odd(X) -> X rem 2 == 1.

filter(F, L) -> [X || X <- L, F(X)].

split([]) ->
    [[],[]];
split(L) ->
    {filter(fun(X) -> even(X) end, L),
     filter(fun(X) -> odd(X) end, L)}.

splitacc([]) ->
    [[], []];
splitacc(L) ->
    splitacc(L, {[], []}).

splitacc([], {Even, Odd}) ->
    {lists:reverse(Even), lists:reverse(Odd)};
splitacc([H|T], {Even, Odd}) ->
    case (H rem 2) of
        0 -> splitacc(T, {[H|Even], Odd});
        1 -> splitacc(T, {Even, [H|Odd]})
    end.
