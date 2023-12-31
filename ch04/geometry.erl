-module(geometry).
-export([area/1, perimeter/1]).

area({square, S}) ->
    S * S;
area({rectangle, W, H}) ->
    W * H;
area({circle, R}) ->
    R * R * math:pi();
area({rightangled_triangle, A, B, _}) ->
    A * B / 2.

perimeter({square, S}) ->
    4 * S;
perimeter({rectangle, W, H}) ->
    2 * (W + H);
perimeter({circle, R}) ->
    R * math:pi();
perimeter({rightangled_triangle, A, B, C}) ->
    A + B + C.
