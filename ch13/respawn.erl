-module(respawn).
-export([spawn_canary/1, keep_alive/1]).

keep_alive(Name) ->
    spawn(fun() ->
                  Ref = monitor(process, Name),
                  receive
                      {'DOWN', Ref, process, _, _} ->
                          spawn_canary(Name)
                  end
          end).

spawn_canary(Name) ->
    Pid = spawn(fun canary/0),
    register(Name, Pid).

canary() ->
    receive
        die ->
            io:format("I'm dying~n")
    after
        5000 ->
            io:format("I'm still running~n"),
            canary()
    end.
