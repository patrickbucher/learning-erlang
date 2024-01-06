-module(parse).
-export([parse_match_result/1]).
-include("parse.hrl").

-spec parse_match_result(string()) -> #result{}.
parse_match_result(Result) ->
    {ok, Pattern} = re:compile("^(.+) ([0-9])+:([0-9]+) (.+)$"),
    {match, [HomeTeam, HomeGoalsStr, AwayGoalsStr, AwayTeam]} =
        re:run(Result, Pattern, [{capture, all_but_first, list}]),
    {HomeGoals, _} = string:to_integer(HomeGoalsStr),
    {AwayGoals, _} = string:to_integer(AwayGoalsStr),
    #result{home_team=HomeTeam, away_team=AwayTeam, home_goals=HomeGoals, away_goals=AwayGoals}.
