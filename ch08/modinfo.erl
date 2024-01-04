-module(modinfo).
-export([most_exports/0, most_common/0, unambiguous_names/0]).

most_exports() ->
    ModuleExportN = lists:map(fun({M, E}) -> {M, length(E)} end, module_exports()),
    topmost(ModuleExportN).

most_common() ->
    Functions = lists:foldl(fun({_, Exports}, Acc) -> Acc ++ Exports end, [], module_exports()),
    FunctionNames = lists:map(fun({Name, _}) -> Name end, Functions),
    FunctionCounts = count(FunctionNames),
    topmost(FunctionCounts).

unambiguous_names() ->
    Functions = lists:foldl(fun({_, Exports}, Acc) -> Acc ++ Exports end, [], module_exports()),
    FunctionNames = lists:map(fun({Name, _}) -> Name end, Functions),
    FunctionCounts = count(FunctionNames),
    UniqueFunctions = lists:filter(fun({_, N}) -> N =:= 1 end, FunctionCounts),
    lists:map(fun({Name, _}) -> Name end, UniqueFunctions).

topmost(ElemCountList) ->
    Ordered = lists:sort(fun({_, LN}, {_, RN}) -> LN > RN end, ElemCountList),
    lists:nth(1, Ordered).

count(List) -> count(List, maps:new()).

count([], Acc) -> maps:to_list(Acc);
count([H|T], Acc) ->
    case maps:is_key(H, Acc) of
        true -> count(T, Acc#{ H := maps:get(H, Acc) + 1 });
        false -> count(T, Acc#{ H => 1 })
    end.

module_exports() ->
    Modules = maps:from_list(code:all_loaded()),
    ModuleNames = maps:keys(Modules),
    lists:map(fun(M) -> {M, apply(M, module_info, [exports])} end, ModuleNames).
