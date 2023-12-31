-module(math_functions).
-export([even/1, odd/1]).

even(X) -> X rem 2 == 0.
odd(X) -> X rem 2 == 1.
