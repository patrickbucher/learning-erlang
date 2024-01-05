-module(grading).
-export([grade/2]).

grade(Points, MaxPoints) when MaxPoints > Points ->
    Points / MaxPoints * 5 + 1.
