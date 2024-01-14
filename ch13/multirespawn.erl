-module(multirespawn).
-export([spawn_canaries/1, keep_alive/1]).

keep_alive(PidsIndexed) ->
    spawn(fun() ->
                  Refs = maps:map(fun(Pid, _) -> monitor(process, Pid) end, PidsIndexed),
                  resurrection_loop(PidsIndexed, Refs)
          end).

resurrection_loop(PidsIndexed, Refs) ->
    receive
        {'DOWN', _, process, Pid, _} ->
            Index = maps:get(Pid, PidsIndexed),
            io:format("resurrect canary ~b~n", [Index]),
            NewPid = spawn(fun() -> canary(Index) end),
            NewRefs = [monitor(process, NewPid) | Refs],
            Others = maps:without([Pid], PidsIndexed),
            NewPidsIndexed = Others#{NewPid => Index},
            resurrection_loop(NewPidsIndexed, NewRefs)
    end.

spawn_canaries(N) ->
    PidsIndexed = [{spawn(fun() -> canary(I) end), I} || I <- for(0, N, [], fun(_, J) -> J end)],
    maps:from_list(PidsIndexed).

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
