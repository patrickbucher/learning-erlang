#!/usr/bin/env escript

main(_) ->
    ModuleInfo = maps:from_list(dict:module_info()),
    Exports = maps:get(exports, ModuleInfo),
    Functions = lists:map(fun({Name, Arity}) -> io_lib:format("~s/~b", [Name, Arity]) end, Exports),
    N = length(Functions),
    io:format("the module dict contains ~b functions: ~p~n", [N, Functions]).
