-module(ring).
-export([benchmark/3, forward/3]).

benchmark(N, M, Debug) ->
    % create and run n+1 nodes (one pseudo)
    NodeIDs = lists:seq(0, N),
    Successors = lists:zip(NodeIDs, tl(NodeIDs), {pad, {N + 1, 0}}),
    PidPairs = lists:foldr(fun(Pair, Acc) -> connect(Pair, Acc, Debug) end, [], Successors),

    % initiate the ring call
    {_ , First} = hd(PidPairs),
    statistics(wall_clock),
    lists:foreach(fun(I) -> ring_call(First, I) end, lists:seq(1, M)),

    % drain the inbox and measure time
    receive
        _ ->
            {_, Time} = statistics(wall_clock),
            io:format("forwarded ~b messages ~b times in ~fs~n", [M, N, Time / 1000])
    end.

ring_call(Pid, I) ->
    Pid ! io_lib:format("hello ~b", [I]).

connect({L, 0}, Acc, _) ->
    [{L, self()} | Acc];
connect({L, _}, Acc, Debug) ->
    [{_, Succ} | _] = Acc,
    [{L, spawn(ring, forward, [L, Succ, Debug])} | Acc].

forward(ID, Succ, Debug) ->
    receive
        Message ->
            if 
                Debug -> io:format("'~s' forwarded from ~b to ~b~n", [Message, ID-1, ID]);
                true -> ok
            end,
            Succ ! Message
    end,
    forward(ID, Succ, Debug).

