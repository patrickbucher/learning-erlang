-module(math_functions).
-export([even/1, odd/1, filter/2]).

even(X) -> X rem 2 == 0.
odd(X) -> X rem 2 == 1.

filter(F, L) -> [X || X <- L, F(X)].
