-module(soccer_table).
-export([compute_table/1, output_table/1]).
-record(result, {home_team="", away_team="", home_goals=0, away_goals=0}).
-record(row, {rank=0, name="", wins=0, defeats=0, ties=0, scored=0, conceded=0, diff=0, points=0}).
-type table() :: [#row{}].

-spec output_table(table()) -> ok.
output_table(Rows) ->
    Title = io_lib:format("~3s ~32s ~3s ~3s ~3s ~3s ~3s ~3s ~3s",
                          ["#", "Team", "W", "T", "L", "+", "-", "=", "P"]),
    Sep = string:copies("-", string:length(Title)),
    Lines = lists:map(fun format_row/1, Rows),
    lists:foreach(fun(L) -> io:format("~s~n", [L]) end, [Title, Sep | Lines]).

-spec format_row(#row{}) -> string().
format_row(#row{} = R) ->
    io_lib:format("~3B ~32ts ~3B ~3B ~3B ~3B ~3B ~3B ~3B",
                  [R#row.rank, R#row.name, R#row.wins, R#row.ties, R#row.defeats,
                   R#row.scored, R#row.conceded, R#row.diff, R#row.points]).

-spec compute_table(string()) -> [#row{}].
compute_table(File) ->
    Results = parse_matchfile(File),
    SingleRows = lists:flatten(lists:map(fun to_rows/1, Results)),
    TableRows = lists:foldl(fun accumulate_rows/2, maps:new(), SingleRows),
    SortedRows = lists:sort(fun sort_rows/2, maps:values(TableRows)),
    RankedRows = lists:map(fun({I, R}) -> R#row{rank=I} end, lists:enumerate(SortedRows)),
    RankedRows.

-spec sort_rows(#row{}, #row{}) -> bool().
sort_rows(#row{name=LN, wins=LW, diff=LD, points=LP}, #row{name=RN, wins=RW, diff=RD, points=RP}) ->
    {LP, LD, LW, RN} > {RP, RD, RW, LN}.

-type row_by_name() :: #{string() => #row{}}.
-spec accumulate_rows(#row{}, row_by_name()) -> row_by_name().
accumulate_rows(#row{name=Name} = New, Acc) ->
    case maps:is_key(Name, Acc) of
        true -> 
            Old = maps:get(Name, Acc),
            Acc#{
              Name := 
              #row{
                 name = Name,
                 wins = New#row.wins + Old#row.wins,
                 defeats = New#row.defeats + Old#row.defeats,
                 ties = New#row.ties + Old#row.ties,
                 scored = New#row.scored + Old#row.scored,
                 conceded = New#row.conceded + Old#row.conceded,
                 diff = New#row.diff + Old#row.diff,
                 points = New#row.points + Old#row.points
                }
             };
        false -> Acc#{Name => New}
    end.

-spec to_rows(#result{}) -> [#row{}].
to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG > AG ->
    [#row{name=HT, wins=1, scored=HG, conceded=AG, diff=HG-AG, points=3},
     #row{name=AT, defeats=1, scored=AG, conceded=HG, diff=AG-HG, points=0}];
to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG < AG ->
    [#row{name=HT, defeats=1, scored=HG, conceded=AG, diff=HG-AG, points=0},
     #row{name=AT, wins=1, scored=AG, conceded=HG, diff=AG-HG, points=3}];
to_rows(#result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}) when HG =:= AG ->
    [#row{name=HT, ties=1, scored=HG, conceded=AG, diff=HG-AG, points=1},
     #row{name=AT, ties=1, scored=AG, conceded=HG, diff=AG-HG, points=1}].

-spec parse_matchfile(string()) -> [#result{}].
parse_matchfile(File) ->
    {ok, Data} = file:read_file(File),
    Lines = string:split(Data, "\n", all),
    NonEmptyLines = lists:filter(fun(L) -> string:length(L) > 0 end, Lines),
    {ok, Pattern} = re:compile("^(.+) ([0-9]+):([0-9]+) (.+)$"),
    lists:map(fun(L) -> to_result(L, Pattern) end, NonEmptyLines).

-spec to_result(string(), re:mp()) -> #result{}.
to_result(Line, Pattern) ->
    {match, [HT, HGStr, AGStr, AT]} = re:run(Line, Pattern, [{capture, all_but_first, binary}]),
    {HG, _} = string:to_integer(HGStr),
    {AG, _} = string:to_integer(AGStr),
    #result{home_team=HT, away_team=AT, home_goals=HG, away_goals=AG}.
