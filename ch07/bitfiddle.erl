-module(bitfiddle).
-export([reverse_bytes/1]).

% usage:
% > bitfiddle:reverse_bytes(<<"abcdef">>).
% <<"fedcba">>
% > bitfiddle:reverse_bytes(<<"fedcba">>).
% <<"abcdef">>
reverse_bytes(Bytes) when is_binary(Bytes) ->
    ByteList = [ X || <<X:8>> <= Bytes ],
    Reversed = lists:reverse(ByteList),
    << <<X>> || X <- Reversed >>.
