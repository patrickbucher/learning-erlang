-module(try_test).
-export([demo/0, catcher/1]).

demo() ->
    [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {"An exception was thrown.", {exceptionThrown, X}};
        exit:X -> {"A fatal error occurred.", {exitCalled, X}};
        error:X -> {"An error occurred.", {errorOccurred, X}}
    end.

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).
