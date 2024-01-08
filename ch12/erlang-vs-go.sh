#!/usr/bin/bash

N=100000
M=10
DEBUG=false

erlc ring.erl
erl -noshell -eval "ring:benchmark($N, $M, $DEBUG)" -s init stop
go run ring.go $N $M $DEBUG
