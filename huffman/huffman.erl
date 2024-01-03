-module(huffman).
-export([compress/1]).

compress(String) ->
    Tree = build_huffman_tree(String),
    Payload = compress(list_to_binary(String), Tree, <<>>),
    {Payload, Tree}.

compress(<<>>, _, Acc) -> Acc;
compress(<<H, T/bitstring>>, Tree, Acc) ->
    Bits = encode(H, Tree, <<>>),
    compress(T, Tree, <<Acc/bitstring, Bits/bitstring>>).

encode(_, {_, _, {}, {}}, Acc) -> Acc;
encode(C, {_, _, {LC, _, _, _} = L, {RC, _, _, _} = R}, Acc) ->
    Left = lists:member(C, binary_to_list(LC)),
    Right = lists:member(C, binary_to_list(RC)),
    if
        Left -> encode(C, L, <<Acc/bitstring, 1:1>>);
        Right -> encode(C, R, <<Acc/bitstring, 0:1>>);
        true -> {error, io_lib:format("~s not found in tree", C)}
    end.

build_huffman_tree(String) ->
    Freqs = freq_asc(String),
    Nodes = lists:map(fun({C, F}) -> {C, F, {}, {}} end, Freqs),
    hd(build_tree(Nodes)).

build_tree([]) -> [];
build_tree(Nodes) when length(Nodes) =:= 1 -> Nodes;
build_tree([{CA, FA, _, _} = L, {CB, FB, _, _} = R | T]) ->
    Tree = [{<<CA/binary, CB/binary>>, FA + FB, L, R} | T],
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
