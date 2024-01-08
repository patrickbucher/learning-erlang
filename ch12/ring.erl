-module(ring).
-export([benchmark/3, forward/3]).

benchmark(N, M, Debug) ->
    case whereis(controller) of
        undefined -> register(controller, self());
        _ -> ok
    end,
    NodeIDs = lists:seq(0, N),
    Successors = lists:zip(NodeIDs, tl(NodeIDs), {pad, {N + 1, 0}}),
    SuccessorPids =
    lists:foldr(
      fun({L, R}, Acc) ->
              case R of
                  0 ->
                      [{L, self()} | Acc];
                  _ ->
                      [{_, Succ} | _] = Acc,
                      [{L, spawn(ring, forward, [L, Succ, Debug])} | Acc]
              end
      end, [], Successors),
    PidsByIndex = maps:from_list(SuccessorPids), 
    statistics(wall_clock),
    lists:foreach(fun(I) ->
                          Message = io_lib:format("hello ~b", [I]),
                          maps:get(0, PidsByIndex) ! Message
                  end, lists:seq(1, M)),
    receive
        _ ->
            {_, Time} = statistics(wall_clock),
            io:format("forwarded ~b messages ~b times in ~fs~n", [M, N, Time / 1000])
    end.

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

