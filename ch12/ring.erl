-module(ring).
-export([benchmark/3, forward/1]).

benchmark(N, M, Debug) ->
    case whereis(controller) of
        undefined -> register(controller, self());
        _ -> ok
    end,
    Pids = lists:map(fun(I) -> {I, spawn(ring, forward, [Debug])} end, lists:seq(1, N)),
    PidsByIndex = maps:from_list(Pids),
    statistics(wall_clock),
    lists:foreach(fun(I) ->
                          Message = io_lib:format("hello ~b", [I]),
                          maps:get(1, PidsByIndex) ! {1, PidsByIndex, Message}
                  end, lists:seq(1, M)),
    receive
        done ->
            {_, Time} = statistics(wall_clock),
            io:format("forwarded ~b messages ~b times in ~fs~n", [M, N, Time / 1000])
    end.

forward(Debug) ->
    receive
        {Pred, PidsByIndex, Message} ->
            Me = Pred + 1,
            Succ = Me + 1,
            if 
                Debug -> io:format("'~s' forwarded from ~b to ~b~n", [Message, Pred, Me]);
                true -> ok
            end,
            case maps:is_key(Succ, PidsByIndex) of
                true ->
                    Pid = maps:get(Succ, PidsByIndex),
                    Pid ! {Me, PidsByIndex, Message},
                    forward(Debug);
                false ->
                    whereis(controller) ! done,
                    forward(Debug)
            end
    end.

