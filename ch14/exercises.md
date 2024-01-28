# Exercise 1

_Start two nodes on the same host. Look up the manual page for the `rpc` module.
Perform some remote procedure calls on the two nodes._

Looking up the manual page:

    $ erl -man rpc

First node (on `thinkcentre`):

    $ erl -sname server
    1> c(kvs).
    {ok,kvs}
    2> kvs:start().
    true

Second node (also on `thinkcentre`):

    $ erl -sname client
    1> rpc:call(server@thinkcentre, kvs, store, [day, sunday]).
    true
    2> rpc:call(server@thinkcentre, kvs, lookup, [day]).
    {ok,sunday}
