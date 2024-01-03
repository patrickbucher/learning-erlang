-module(huffman).
-export([build_huffman_tree/1]).

build_huffman_tree(String) ->
    Freqs = freq_asc(String),
    Nodes = lists:map(fun({C, F}) -> {C, F, {}, {}} end, Freqs),
    build_tree(Nodes).

build_tree([]) -> [];
build_tree(Nodes) when length(Nodes) =:= 1 -> Nodes;
build_tree([{CA, FA, _, _} = L, {CB, FB, _, _} = R | T]) ->
    Chars = <<CA/binary, CB/binary>>,
    Freqs = FA + FB,
    Node = {Chars, Freqs, L, R},
    Tree = [Node | T],
    SortedTree = lists:sort(fun({_, LF, _, _}, {_, RF, _, _}) -> LF < RF end, Tree),
    build_tree(SortedTree).

freq_asc(String) ->
    Chars = [ X || X <- String ],
    FreqMap = lists:foldl(fun map_freq/2, maps:new(), Chars),
    FreqList = maps:to_list(FreqMap),
    lists:sort(fun({_, LF}, {_, RF}) -> LF < RF end, FreqList).

map_freq(E, Map) ->
    Key = <<E>>,
    case maps:is_key(Key, Map) of
        true -> Map#{ Key := maps:get(Key, Map) + 1 };
        false -> Map#{ Key => 1 }
    end.
