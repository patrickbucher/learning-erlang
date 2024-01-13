-module(multirespawn).
-export([spawn_canaries/1]).

% TODO: implement keep_alive for indexed pid map

spawn_canaries(N) ->
    IndexedPids = [{I, spawn(fun() -> canary(I) end)} || I <- for(0, N, [], fun(_, J) -> J end)],
    maps:from_list(IndexedPids).

for(N, N, Acc, _) -> lists:reverse(Acc);
for(I, N, Acc, Next) -> for(I + 1, N, [Next(Acc, I) | Acc], Next).

canary(X) ->
    receive
        die ->
            io:format("Canary ~b is dying.~n", [X])
    after
        5000 ->
            io:format("Canary ~b is alive.~n", [X]),
            canary(X)
    end.
