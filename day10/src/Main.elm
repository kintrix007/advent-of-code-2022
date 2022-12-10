module Main exposing (..)

import Html exposing (text)

type Command = Noop | AddX Int
type alias Tick = Int
type alias Strength = Int

main : Html.Html msg
main =
    let
        cmds = List.map parseLine <| String.lines <| String.trim input
        _ = Debug.log "Part 1" <| part1 cmds
        _ = "Part2:\n" ++ part2 cmds |> String.split "\n"
            |> List.reverse
            |> List.map(Debug.log "")
    in
        text "Hello, World!"

part1 : List Command -> Int
part1 cmds =
    let (_, _, s) = List.foldl f (0, 1, 0) cmds
        f = \cmd (tick, x, str) -> execute1 cmd tick x str
    in s

part2 : List Command -> String
part2 cmds =
    let (_, _, s) = List.foldl f (0, 1, "") cmds
        f = \cmd (tick, x, str) -> execute2 cmd tick x str
    in s

execute1 : Command -> Tick -> Int -> Strength -> (Tick, Int, Strength)
execute1 cmd tick x str =
    let apply n t x_ s vs =
            let mod = modBy 40 (t-20)
            in (t, x_+n, s + if List.member mod vs then (t-mod)*x else 0)
    in case cmd of
        Noop -> apply 0 (tick+1) x str [0]
        AddX n -> apply n (tick+2) x str [0, 1]

execute2 : Command -> Tick -> Int -> String -> (Tick, Int, String)
execute2 cmd tick x str =
    case cmd of
        Noop ->
            let mod1 = modBy 40 tick
                mod2 = modBy 40 (tick+1)
            in (tick+1, x, str ++ (if List.member (abs (mod1-x)) [0, 1] then "#" else " ") ++ if mod2 == 0 then "\n" else "")
        AddX n ->
            let mod1 = modBy 40 tick
                mod2 = modBy 40 (tick+1)
                mod3 = modBy 40 (tick+2)
                s1 = (if (abs (mod1-x)) <= 1 then "#" else " ") ++ if mod2 == 0 then "\n" else ""
                s2 = (if (abs (mod2-x)) <= 1 then "#" else " ") ++ if mod3 == 0 then "\n" else ""
            in (tick+2, x+n, str ++ s1 ++ s2)

parseLine : String -> Command
parseLine l =
    case l of
        "noop" -> Noop
        x -> case String.split " " x of 
            [_, amount] -> AddX <| Maybe.withDefault 0 <| String.toInt amount
            _ -> Noop

input : String
input = """
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
