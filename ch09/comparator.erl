-module(comparator).
-export([compare/2]).

-type result() :: smaller | bigger | equal.
-spec compare(Left::number(), Right::number()) -> result().

compare(Left, Right) ->
    if
        Left < Right -> smaller;
        Left > Right -> bigger;
        true -> equal
    end.
