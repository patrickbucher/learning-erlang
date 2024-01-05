-module(colours).
-export([rgb_to_hex/3]).

rgb_to_hex(R, G, B) ->
    io_lib:format("#~2.16.0b~2.16.0b~2.16.0b", [R, G, B]).
