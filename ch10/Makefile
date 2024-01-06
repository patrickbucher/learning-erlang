.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

MODS = parse parse_test

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump

test: parse.beam parse_test.beam
	erl -noshell -s parse_test test_parse_match_result -s init stop
