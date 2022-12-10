module Main exposing (..)

import Html

type Command = Noop | AddX Int
type alias Tick = Int
type alias Strength = Int

main : Html.Html msg
main =
    let
        cmds = input |> String.lines |> List.filterMap parseLine
        _ = part1 cmds |> Debug.log "Part 1"
        _ = "Part2:\n" ++ part2 cmds |> String.split "\n"
            |> List.reverse
            |> List.map (Debug.log "")
    in
        Html.text "Hello, World!"

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
    let mod k = modBy 40 (tick+k)
        chk k = (if (abs (mod k-x)) <= 1 then "##" else "  ") ++ if mod (k+1) == 0 then "\n" else ""
    in case cmd of
        Noop -> (tick+1, x, str ++ chk 0)
        AddX n -> (tick+2, x+n, str ++ chk 0 ++ chk 1)

parseLine : String -> Maybe Command
parseLine l =
    case String.split " " l of
        ["noop"] -> Just Noop
        ["addx", amount] -> Just <| AddX <| Maybe.withDefault 0 <| String.toInt amount
        _ -> Nothing

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
