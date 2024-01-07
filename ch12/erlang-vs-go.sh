#!/usr/bin/bash

N=10
M=3
DEBUG=true

erl -noshell -eval "ring:benchmark($N, $M, $DEBUG)" -s init stop
go run ring.go $N $M $DEBUG
