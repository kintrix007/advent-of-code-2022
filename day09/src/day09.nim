import strutils
import strformat
import sequtils
import fusion/matching
import std/sets
import sugar

type
  Coord = tuple[x: int, y: int]
  Parsed = seq[tuple[dir: Dir, amount: int]]
  Dir = enum
    left, right, up, down

func parseDir(str: string): Dir
func dist(p1, p2: Coord): int

proc part1(parsed: Parsed): int =
  var 
    h = (0, 0).Coord
    t = (0, 0).Coord
    visited = toHashSet [t]
  
  #? Those parentheses DO make a difference
  for (dir, amount) in parsed:
    for _ in 0..<amount:
      case dir:
        of up:    inc h.y
        of down:  dec h.y
        of right: inc h.x
        of left:  dec h.x
      
      let d = h.dist(t)
      
      if d > 1:
        t.x += clamp(h.x - t.x, -1, 1)
        t.y += clamp(h.y - t.y, -1, 1)
        visited.incl t
      
      # echo fmt"H: {h:20}T: {t:20}dist: {d}"

  visited.len

proc part2(parsed: Parsed): int =
  var
    coords = toSeq(1..10).map(x => (0, 0).Coord)
    visited = toHashSet [coords[^1]]
  
  for (dir, amount) in parsed:
    for _ in 0..<amount:
      case dir:
        of up:    inc coords[0].y
        of down:  dec coords[0].y
        of right: inc coords[0].x
        of left:  dec coords[0].x
      
      for i in 1..<coords.len:
        let d = coords[i].dist(coords[i-1])
        if d > 1:
          coords[i].x += clamp(coords[i-1].x - coords[i].x, -1, 1)
          coords[i].y += clamp(coords[i-1].y - coords[i].y, -1, 1)

      visited.incl coords[^1]
      
      # echo fmt"H: {h:20}T: {t:20}dist: {d}"

  visited.len

## Weird implementation of dist for this problem
func dist(p1, p2: Coord): int =
  max(abs(p1.x-p2.x), abs(p1.y-p2.y))

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
  let parsed = parse("input")
  # echo parsed
  let
    p1 = part1 parsed
    p2 = part2 parsed
  
  echo fmt"Part1: {p1}"
  echo fmt"Part2: {p2}"
