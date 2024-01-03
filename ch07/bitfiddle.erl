-module(bitfiddle).
-export([reverse_bytes/1, binary_to_packet/1]).

% usage:
% > bitfiddle:reverse_bytes(<<"abcdef">>).
% <<"fedcba">>
% > bitfiddle:reverse_bytes(<<"fedcba">>).
% <<"abcdef">>
reverse_bytes(Bytes) when is_binary(Bytes) ->
    ByteList = [ X || <<X:8>> <= Bytes ],
    Reversed = lists:reverse(ByteList),
    << <<X>> || X <- Reversed >>.

% usage:
% > bitfiddle:binary_to_packet("abcdef").
% <<0,0,0,10,131,107,0,6,97,98,99,100,101,102>>
binary_to_packet(Term) ->
    Payload = term_to_binary(Term),
    N = byte_size(Payload),
    <<N:(4*8), Payload:N/binary>>.
