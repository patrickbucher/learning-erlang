Run the example:

    $ erl
    > c(afile_server).
    > c(afile_client).
    > FileServer = afile_server:start(".").
    > afile_client:ls(FileServer).
    {ok,["afile_server.erl","Makefile","afile_client.erl"]}
    > afile_client:get_file(FileServer, "Makefile").
    {ok,<<".PHONY: all clean\n\nall: afile_server.beam afile_client.beam\n\n.SUFFIXES: .erl .beam\n.erl.beam:\n\terlc $<\n\nclea"...>>}
    > afile_client:put_file(FileServer, "hello", "world").
    ok
