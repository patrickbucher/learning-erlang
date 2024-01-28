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

# Exercise 3

_Repeat the previous exercise, only with the two nodes on different networks._

Make sure port 4369 (for the _Erlang Port Mapper Daemon_) is open on the server
for **both TCP and UDP**, as well as an additional port range (e.g. 12000 to
13000).

Copy the KVS code to some remote server:

    $ scp kvs.erl debian@wolfsegg:

SSH into the server, install Erlang:

    $ ssh debian@wolfsegg
    $ sudo apt install -y erlang

Starting the server:

    $ erl -sname server -setcookie topsecret \
      -kernel inet_dist_listen_min 12000 inet_dist_listen_max 13000
    1> c(kvs).
    {ok,kvs}
    2> kvs:start(). 
    true

Starting the client:

    $ erl -sname client -setcookie topsecret
    1> rpc:call(server@wolfsegg, kvs, store, [year, 2024]).
    {badrpc,nodedown}

Did not work.

# Exercise 4

_skipped_
