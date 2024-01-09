-module(diemsg).
-export([my_spawn/3, divider/0]).

my_spawn(Mod, Func, Args) ->
    Spawned = secs(),
    Pid = spawn(Mod, Func, Args),
    spawn(fun() ->
                  % the calling process of monitor/2 will receive messages
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          Died = secs(),
                          Diff = Died - Spawned,
                          io:format("PID ~p died after ~f seconds of ~p~n", [Pid, Diff, Why])
                  end
          end),
    Pid.

divider() ->
    receive
        {Dividend, Divisor} ->
            Result = Dividend div Divisor,
            io:format("~b/~b=~b~n", [Dividend, Divisor, Result])
    end,
    divider().

secs() ->
    {Mega, Secs, Millis} = os:timestamp(),
    Mega * 1.0e6 + Secs + Millis / 1.0e6.

