#!/usr/bin/env escript

main(Args) ->
    Table = soccer_table:compute_table(lists:nth(1, Args)),
    soccer_table:output_table(Table).
