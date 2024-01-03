-module(bitfiddle).
-export([reverse_bytes/1, binary_to_packet/1, packet_to_term/1,
         test_reverse_bytes/0, test_packet/0]).

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

% usage:
% > bitfiddle:packet_to_term(<<0,0,0,10,131,107,0,6,97,98,99,100,101,102>>).
% "abcdef"
packet_to_term(Packet) when is_bitstring(Packet) ->
    <<_:(4*8), Payload/binary>> = Packet,
    binary_to_term(Payload).

% usage:
% > bitfiddle:test_reverse_bytes().
% ok
test_reverse_bytes() ->
    Original = <<"hello, world">>,
    Reversed = reverse_bytes(Original),
    ReReversed = reverse_bytes(Reversed),
    ReReversed = Original,
    ok.

% usage:
% > bitfiddle:test_packet().
% ok
test_packet() ->
    Term = {multiplication, 2, 3, 6},
    Packet = binary_to_packet(Term),
    Unpacked = packet_to_term(Packet),
    Term = Unpacked,
    ok.
