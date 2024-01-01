-module(my_maps).
-export([map_search_pred/2]).

map_search_pred(Map, Pred) ->
    List = maps:to_list(Map),
    [First|_] = lists:filter(fun({Key, Val}) -> Pred(Key, Val) end, List),
    maps:from_list([First]).

% usage:
% > c(my_maps).
% > Squares = #{1 => 1, 2 => 4, 3 => 9, 4 => 16}.
% > BothOdd = fun(K, V) -> K rem 2 =:= 1 andalso V rem 2 =:= 1 end.
% > my_maps:map_search_pred(Squares, BothOdd).
% #{1 => 1}
