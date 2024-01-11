-module(dieafter).
-export([my_spawn/4, divider/0]).

my_spawn(Mod, Func, Args, Time) ->
    Spawned = secs(),
    Pid = spawn(Mod, Func, Args),
    spawn(fun() ->
                  % the calling process of monitor/2 will receive messages
                  Ref = monitor(process, Pid),
                  overwatch(Ref, Pid, Spawned, Time)
          end),
    Pid.

overwatch(Ref, Pid, Spawned, Time) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            Died = secs(),
            Diff = Died - Spawned,
            io:format("PID ~p died after ~f seconds of ~p~n", [Pid, Diff, Why])
    after
        Time * 1000 ->
            exit(Pid, timeout)
    end,
    overwatch(Ref, Pid, Spawned, Time).

divider() ->
    receive
        {Dividend, Divisor} ->
            Result = Dividend div Divisor,
            io:format("~b/~b=~b~n", [Dividend, Divisor, Result]),
            divider()
    end.

secs() ->
    {Mega, Secs, Millis} = os:timestamp(),
    Mega * 1.0e6 + Secs + Millis / 1.0e6.

