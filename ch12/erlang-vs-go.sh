#!/usr/bin/bash

N=10000
M=100

erl -noshell -eval "ring:benchmark($N, $M, false)" -s init stop
go run ring.go $N $M false
