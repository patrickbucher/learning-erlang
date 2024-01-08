#!/usr/bin/bash

N=200000
M=100
DEBUG=false

erlc ring.erl
erl -noshell -eval "ring:benchmark($N, $M, $DEBUG)" -s init stop
go build ring.go
./ring $N $M $DEBUG
