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

# Exercise 2

_Repeat the previous exercise, only with the two nodes on the same LAN._

Starting an existing Debian 12 VM using libvirt:

    $ sudo virsh start bookworm

Copy the KVS code to the VM:

    $ scp kvs.erl bookworm:

SSH into the VM:

    $ ssh bookworm

Setting up Erlang:

    $ sudo apt install -y erlang

Starting the server (on the VM):

    $ erl -sname server -setcookie topsecret
    1> c(kvs).
    {ok,kvs}
    2> kvs:start().
    true

Starting the client (on the host):

    $ erl -sname client -setcookie topsecret

Use the KVS from the client:

    1> rpc:call(server@bookworm, kvs, store, [month, january]).
    true
    2> rpc:call(server@bookworm, kvs, lookup, [month]).
    {ok, january}
