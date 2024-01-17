-module(kvs).
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop(#{}) end)).

store(Key, Value) -> call({store, Key, Value}).

lookup(Key) -> call({lookup, Key}).

call(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs, Reply} ->
            Reply
    end.

loop(Store) ->
    receive
        {From, {store, Key, Value}} ->
            UpdatedStore = maps:put(Key, Value, Store),
            From ! {kvs, true},
            loop(UpdatedStore);
        {From, {lookup, Key}} ->
            case maps:is_key(Key, Store) of
                true ->
                    From ! {kvs, {ok, maps:get(Key,Store)}};
                false ->
                    From ! {kvs, undefined}
            end
    end,
    loop(Store).
