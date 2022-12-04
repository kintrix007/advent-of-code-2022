-module(cleanup).
-export([main/0]).

main() ->
    {ok, RawContents} = file:read_file("input"),
    Lines = string:split(string:trim(RawContents), "\n", all),
    Pairs = [ parse_line(L) || L <- Lines ],
    % io:format("~p~n", [Pairs]),
    Part1 = part1(Pairs),
    Part2 = part2(Pairs),
    io:format("Part 1: ~p~nPart 2: ~p~n", [Part1, Part2]).

part1(Ranges) ->
    lists:sum([ part1_line(R1, R2) || {R1, R2} <- Ranges ]).

part1_line(First, Second) ->
    Contains = range_contains(First, Second) or range_contains(Second, First),
    case Contains of
        true -> 1;
        false -> 0
    end.

part2(Ranges) ->
    lists:sum([ part2_line(R1, R2) || {R1, R2} <- Ranges ]).

part2_line(First, Second) ->
    Contains = range_overlaps(First, Second),
    case Contains of
        true -> 1;
        false -> 0
    end.

range_overlaps(Range, SubRange) ->
    {range, S1, E1} = Range,
    {range, S2, E2} = SubRange,
    (S1 =< E2) and (E1 >= S2).

range_contains(Range, SubRange) ->
    {range, S1, E1} = Range,
    {range, S2, E2} = SubRange,
    (S1 =< S2) and (E2 =< E1). 

parse_line(Line) ->
    [FirstStr, SecondStr] = string:split(Line, ","),
    [FirstStartStr, FirstEndStr] = string:split(FirstStr, "-"),
    [SecondStartStr, SecondEndStr] = string:split(SecondStr, "-"),
    {FirstStart, FirstEnd} = {binary_to_integer(FirstStartStr), binary_to_integer(FirstEndStr)},
    {SecondStart, SecondEnd} = {binary_to_integer(SecondStartStr), binary_to_integer(SecondEndStr)},
    First = {range, FirstStart, FirstEnd},
    Second = {range, SecondStart, SecondEnd},
    {First, Second}.
