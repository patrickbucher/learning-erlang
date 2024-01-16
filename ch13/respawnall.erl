-module(respawnall).
-export([spawn_canaries/1, keep_alive/1]).

keep_alive(PidsIndexed) ->
    spawn(fun() ->
                  maps:map(fun(Pid, _) -> monitor(process, Pid) end, PidsIndexed),
                  resurrection_loop(PidsIndexed)
          end).

resurrection_loop(PidsIndexed) ->
    receive
        {'DOWN', _, process, _, _} ->
            io:format("resurrect all canaries~n"),
            NewPidsIndexed = lists:map(fun(I) ->
                                               {spawn(fun() -> canary(I) end), I}
                                       end, maps:values(PidsIndexed)),
            lists:foreach(fun({Pid, _}) -> monitor(process, Pid) end, NewPidsIndexed),
            resurrection_loop(NewPidsIndexed)
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
