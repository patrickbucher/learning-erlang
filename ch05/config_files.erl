-module(config_files).
-export([read_json_config/1]).

read_json_config(Path) ->
    Content = slurp(Path),
    maps:from_json(Content). % NOTE: this function does not exist!

slurp(Path) ->
    {ok, Device} = file:open(Path, [read]),
    Content = lists:reverse(slurp(Device, [])),
    ok = file:close(Device),
    string:join(Content, "~n").

slurp(Device, Lines) ->
    case io:get_line(Device, "") of
        eof -> Lines;
        Line -> slurp(Device, [Line|Lines])
    end.
