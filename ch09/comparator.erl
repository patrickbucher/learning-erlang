-module(comparator).
-export([compare/2]).

compare(Left, Right) ->
    if
        Left < Right -> smaller;
        Left > Right -> bigger;
        true -> equal
    end.
