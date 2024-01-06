-module(parse_test).
-export([test_parse_match_result/0]).
-import(parse, [parse_match_result/1]).
-include("parse.hrl").

test_parse_match_result() ->
    Expected = #result{home_team="FCL", away_team="FCB", home_goals=3, away_goals=1},
    Expected = parse:parse_match_result("FCL 3:1 FCB"),
    ok.
