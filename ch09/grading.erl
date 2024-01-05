-module(grading).
-export([grade/2]).

-type swiss_grade() :: float().
-spec grade(Points::number(), MaxPoints::number()) -> swiss_grade().

grade(Points, MaxPoints) when MaxPoints > Points ->
    Points / MaxPoints * 5 + 1.
