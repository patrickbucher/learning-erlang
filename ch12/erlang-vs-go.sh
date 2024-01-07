#!/usr/bin/bash

N=10000
M=3
DEBUG=false

erlc ring.erl
erl -noshell -eval "ring:benchmark($N, $M, $DEBUG)" -s init stop
go run ring.go $N $M $DEBUG
