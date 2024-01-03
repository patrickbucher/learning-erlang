-module(huffman).
-export([compress/1, decompress/2, compress_file/2, decompress_file/2]).

compress_file(Source, Destination) ->
    {ok, Data} = file:read_file(Source),
    Compressed = compress(Data),
    Payload = term_to_binary(Compressed),
    file:write_file(Destination, Payload).

decompress_file(Source, Destination) ->
    {ok, Data} = file:read_file(Source),
    {Payload, Tree} = binary_to_term(Data),
    Decompressed = decompress(Payload, Tree),
    file:write_file(Destination, Decompressed).

decompress(Payload, Tree) ->
    decode(Payload, Tree, Tree, <<>>).

decode(<<>>, _, {C, _, _, _}, Acc) -> <<Acc/binary, C/binary>>;
decode(Payload, Tree, {C, _, {}, {}}, Acc) ->
    decode(Payload, Tree, Tree, <<Acc/binary, C/binary>>);
decode(<<Step:1, Rest/bitstring>>, Tree, {_, _, L, R}, Acc) ->
    case Step of
        0 -> decode(Rest, Tree, L, Acc);
        1 -> decode(Rest, Tree, R, Acc)
    end.

compress(Bin) ->
    Tree = build_huffman_tree(Bin),
    Payload = compress(Bin, Tree, <<>>),
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
        Left -> encode(C, L, <<Acc/bitstring, 0:1>>);
        Right -> encode(C, R, <<Acc/bitstring, 1:1>>);
        true -> {error, io_lib:format("~s not found in tree", C)}
    end.

build_huffman_tree(Bin) ->
    Freqs = freq_asc(Bin),
    Nodes = lists:map(fun({C, F}) -> {C, F, {}, {}} end, Freqs),
    hd(build_tree(Nodes)).

build_tree([]) -> [];
build_tree(Nodes) when length(Nodes) =:= 1 -> Nodes;
build_tree([{CA, FA, _, _} = L, {CB, FB, _, _} = R | T]) ->
    Tree = [{<<CA/binary, CB/binary>>, FA + FB, L, R} | T],
    SortedTree = lists:sort(fun({_, LF, _, _}, {_, RF, _, _}) -> LF < RF end, Tree),
    build_tree(SortedTree).

freq_asc(Bin) ->
    Chars = [ X || X <- binary_to_list(Bin) ],
    FreqMap = lists:foldl(fun map_freq/2, maps:new(), Chars),
    FreqList = maps:to_list(FreqMap),
    lists:sort(fun({_, LF}, {_, RF}) -> LF < RF end, FreqList).

map_freq(E, Map) ->
    Key = <<E>>,
    case maps:is_key(Key, Map) of
        true -> Map#{ Key := maps:get(Key, Map) + 1 };
        false -> Map#{ Key => 1 }
    end.
