-module(spawntime).
-export([csv_stats/2, stats/1, max/1]).

csv_stats(Steps, File) ->
    Stats = stats(Steps),
    {ok, Output} = file:open(File, write),
    io:fwrite(Output, "~s,~s,~s~n", ["n", "cpu-time", "wall-time"]),
    lists:foreach(fun([N, CT, WT]) -> io:fwrite(Output, "~b,~f,~f~n", [N, CT, WT]) end, Stats),
    file:close(Output).

stats(Steps) ->
    RealMax = erlang:system_info(process_limit),
    SoftMax = floor((RealMax div 100) * 100),
    StepSize = SoftMax div Steps,
    Ns = lists:seq(StepSize, SoftMax, StepSize),
    Timings = lists:map(fun(N) -> [N | tuple_to_list(max(N))] end, Ns),
    Timings.

max(N) ->
    statistics(runtime),
    statistics(wall_clock),
    Pids = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Runtime} = statistics(runtime),
    {_, Clock} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, Pids),
    {Runtime * 1000 / N, Clock * 1000 / N}.

wait() ->
    receive
        die -> void
    end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
