import strutils, strformat
import sequtils
import std/sets
import sugar
import fusion/matching

type
  Coord = tuple[x: int, y: int]
  Parsed = seq[tuple[dir: Dir, amount: int]]
  Dir = enum
    left, right, up, down

proc parse(filename: string): Parsed
func parseDir(str: string): Dir
func drag(head: Coord, tail: var Coord)
func move(p: var Coord, dir: Dir)
func moveRope(parsed: Parsed, knots: int): int

proc part1(parsed: Parsed): int =
  moveRope(parsed, 2)

func part2(parsed: Parsed): int =
  moveRope(parsed, 10)


func moveRope(parsed: Parsed, knots: int): int =
  var
    coords = toSeq(1..knots).map(x => (0, 0).Coord)
    visited = toHashSet [coords[^1]]

  for (dir, amount) in parsed:
    for _ in 0..<amount:
      coords[0].move dir
      for i in 1..<knots:
        coords[i-1].drag coords[i]

      visited.incl coords[^1]

  visited.len

func dist(p1, p2: Coord): int =
  ## Weird implementation of dist for this problem
  max(abs(p1.x-p2.x), abs(p1.y-p2.y))

func drag(head: Coord, tail: var Coord) =
  let d = head.dist(tail)
  if d > 1:
    tail.x += clamp(head.x - tail.x, -1, 1)
    tail.y += clamp(head.y - tail.y, -1, 1)

func move(p: var Coord, dir: Dir) =
  case dir:
    of up:    inc p.y
    of down:  dec p.y
    of right: inc p.x
    of left:  dec p.x

proc parse(filename: string): Parsed =
  let cont = filename.readFile().strip()
  cont.splitLines().map do (x: string) -> auto:
    [@dirStr, @amountStr] := x.split(" ")
    let
      dir = dirStr.parseDir()
      amount = amountStr.parseInt()
    (dir: dir, amount: amount)

func parseDir(str: string): Dir =
  case str:
    of "L": left
    of "R": right
    of "U": up
    of "D": down
    else: raise newException(ValueError, "Wrong dir '%s'" % [str])


when isMainModule:
  let
    parsed = parse("input")
    p1 = part1 parsed
    p2 = part2 parsed

  echo fmt"Part1: {p1}"
  echo fmt"Part2: {p2}"
