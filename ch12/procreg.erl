-module(procreg).
-export([start/2, demo/0]).

start(AnAtom, Fun) ->
    case whereis(AnAtom) of
        undefined ->
            Pid = spawn(Fun),
            register(AnAtom, Pid);
        Pid ->
            io:format("use ~p~n", [Pid])
    end.

demo() ->
    Fun = fun() -> io:format("hello from ~p~n", [self()]) end,
    spawn(procreg, start, [demo, Fun]),
    spawn(procreg, start, [demo, Fun]),
    ok.
