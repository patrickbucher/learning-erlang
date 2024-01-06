-module(parsing).
-export([parse_equation/2]).

-spec parse_equation(string(), re:mp()) -> {binary(), binary()}.
parse_equation(Equation, Pattern) ->
    {match, [_, Left, Right]} = re:run(Equation, Pattern, [{capture, all, binary}]),
    {Left, Right}.
