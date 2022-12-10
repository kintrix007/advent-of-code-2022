module Main exposing (..)

import Html exposing (text)

type Command = Noop | AddX Int
type alias Tick = Int
type alias Strength = Int

main : Html.Html msg
main =
    let
        cmds = parse input
        _ = Debug.log "dat" cmds
        _ =  Debug.log "Part 1" <| part1 <| cmds
        _ =  Debug.log "Part 2" <| part2 <| cmds
    in
        text "Hello, World!"

part1 : List Command -> Int
part1 cmds =
    executeAll cmds 0 1

part2 : List Command -> Int
part2 data = -1

executeAll : List Command -> Tick -> Int -> Int
executeAll cmds tick x =
    case cmds of
        [] -> 0
        cmd::rest ->
            let
                (newTick, newX, str) = execute cmd tick x 0
                _ = Debug.log "str" str
            in
                str + executeAll rest newTick newX

execute : Command -> Tick -> Int -> Strength -> (Tick, Int, Strength)
execute cmd tick x str =
    case cmd of
        Noop ->
            let newTick = tick+1
            in (newTick, x, if modBy 40 (newTick-20) == 0 then newTick*x else str)
        AddX n ->
            let newTick = tick+2
                mod = modBy 40 (newTick-20)
            in (newTick, x+n, if mod == 0 || mod == 1 then (newTick-mod)*x else str)


parse : String -> List Command
parse str =
    List.map parseLine <| String.lines str

parseLine : String -> Command
parseLine l =
    case l of
        "noop" -> Noop
        x -> case String.split " " x of 
            [_, amount] -> AddX <| Maybe.withDefault 0 <| String.toInt amount
            _ -> Noop

input : String
input = String.trim """
addx 1
noop
addx 4
noop
noop
addx 7
noop
noop
noop
addx 3
noop
noop
addx 5
addx -1
addx 1
addx 5
addx 3
noop
addx 3
noop
addx -1
noop
addx 3
addx 5
addx -38
addx 7
addx 10
addx -14
addx 5
addx 30
addx -25
noop
addx 2
addx 3
addx -2
addx 2
addx 5
addx 2
addx 2
addx -21
addx 22
addx 5
addx 2
addx 3
noop
addx -39
addx 1
noop
noop
addx 3
addx 5
addx 4
addx -5
addx 4
addx 4
noop
addx -9
addx 12
addx 5
addx 2
addx -1
addx 6
addx -2
noop
addx 3
addx 3
addx 2
addx -37
addx 39
addx -33
addx -1
addx 1
addx 8
noop
noop
noop
addx 2
addx 20
addx -19
addx 4
noop
noop
noop
addx 3
addx 2
addx 5
noop
addx 1
addx 4
addx -21
addx 22
addx -38
noop
noop
addx 7
addx 32
addx -27
noop
addx 3
addx -2
addx 2
addx 5
addx 2
addx 2
addx 3
addx -2
addx 2
noop
addx 3
addx 5
addx 2
addx 3
noop
addx -39
addx 2
noop
addx 4
addx 8
addx -8
addx 6
addx -1
noop
addx 5
noop
noop
noop
addx 3
addx 5
addx 2
addx -11
addx 12
addx 2
noop
addx 3
addx 2
addx 5
addx -6
noop
"""
