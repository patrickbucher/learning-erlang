#!/usr/bin/env escript

main(_) ->
    % {house, number, storeys, rooms}
    VanHoutens = {house, 1, 2, 6},
    Simpsons = {house, 2, 2, 5},
    Flanders = {house, 3, 3, 7},
    Muntzes = {house, 4, 1, 4},
    EvergreenTerrace = [VanHoutens, Simpsons, Flanders, Muntzes],

    [_, _, _, {house, _, _, MuntzesRooms}] = EvergreenTerrace,
    [_, {house, _, SimpsonsStoreys, _}|_] = EvergreenTerrace,

    io:format("number of rooms in Muntze's house: ~B~n", [MuntzesRooms]),
    io:format("number of storeys in Simpson's house: ~B~n", [SimpsonsStoreys]).
