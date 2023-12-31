.PHONY: all clean

all: afile_server.beam afile_client.beam

.SUFFIXES: .erl .beam
.erl.beam:
	erlc $<

clean:
	rm -f *.beam
