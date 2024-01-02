-module(soccer_table).
-export([compute_table/1]).
-record(result, {home_team="", away_team="", home_goals=0, away_goals=0}).
-record(row, {rank=0, name="", wins=0, defeats=0, ties=0, scored=0, conceded=0, diff=0, points=0}).

compute_table(File) ->
    Results = parse_matchfile(File),
    SingleRows = lists:flatten(lists:map(fun(R) -> to_rows(R) end, Results)),
    SingleRows. % todo: accumulate single rows

to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG > AG ->
    [#row{name=HT, wins=1, scored=HG, conceded=AG, diff=HG-AG, points=3},
     #row{name=AT, defeats=1, scored=AG, conceded=HG, diff=AG-HG, points=0}];
to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG < AG ->
    [#row{name=HT, defeats=1, scored=HG, conceded=AG, diff=HG-AG, points=0},
     #row{name=AT, wins=1, scored=AG, conceded=HG, diff=AG-HG, points=3}];
to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG =:= AG ->
    [#row{name=HT, ties=1, scored=HG, conceded=AG, diff=HG-AG, points=1},
     #row{name=AT, ties=1, scored=AG, conceded=HG, diff=AG-HG, points=1}].

parse_matchfile(File) ->
    {ok, Data} = file:read_file(File),
    Lines = string:split(Data, "\n", all),
    NonEmptyLines = lists:filter(fun(L) -> string:length(L) > 0 end, Lines),
    {ok, Pattern} = re:compile("^(.+) ([0-9]+):([0-9]+) (.+)$"),
    lists:map(fun(L) -> to_result(L, Pattern) end, NonEmptyLines).

to_result(Line, Pattern) ->
    {match, [HT, HGStr, AGStr, AT]} = re:run(Line, Pattern, [{capture, all_but_first, binary}]),
    {HG, _} = string:to_integer(HGStr),
    {AG, _} = string:to_integer(AGStr),
    #result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}.
