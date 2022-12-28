package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

type (
	Coord struct {
		x, y int
	}
	Table map[Coord]bool
)

type Direction int

const (
	Stay Direction = iota
	North
	South
	West
	East
)

func solve(t Table) (emptyTiles, lastRound int) {
	defer func() { lastRound++ }()

	directionOrder := []Direction{North, South, West, East}

	for ; lastRound < 10; lastRound++ {
		t, directionOrder = doIteration(t, directionOrder)
	}

	min, max := t.Area()

	for y := min.y; y <= max.y; y++ {
		for x := min.x; x <= max.x; x++ {
			if _, ok := t[Coord{x, y}]; !ok {
				emptyTiles++
			}
		}
	}

	for ; !allStay(t, directionOrder); lastRound++ {
		t, directionOrder = doIteration(t, directionOrder)
	}

	return
}

func allStay(t Table, directionOrder []Direction) bool {
	for coord := range t {
		dir := getMoveDirection(t, coord, directionOrder)
		if dir != Stay {
			return false
		}
	}
	return true
}

func doIteration(t Table, directionOrder []Direction) (Table, []Direction) {
	targetToCoord := map[Coord]Coord{}
	coordToTarget := map[Coord]Coord{}

	for coord := range t {
		dir := getMoveDirection(t, coord, directionOrder)
		target := coord.Add(dir.ToCoord())
		if from, ok := targetToCoord[target]; ok {
			delete(coordToTarget, from)
			continue
		}
		targetToCoord[target] = coord
		coordToTarget[coord] = target
	}

	for coord, target := range coordToTarget {
		delete(t, coord)
		t[target] = true
	}

	return t, append(directionOrder[1:], directionOrder[0])
}

func getMoveDirection(t Table, coord Coord, directionOrder []Direction) (res Direction) {
	res = Stay
	amountValid := 0

	for _, dir := range directionOrder {
		if checkDirection(t, coord, dir) {
			if amountValid == 0 {
				res = dir
			}
			amountValid++
		}
	}

	if amountValid == len(directionOrder) {
		res = Stay
	}
	return
}

func checkDirection(t Table, c Coord, direction Direction) (isFree bool) {
	c = c.Add(direction.ToCoord())

	switch direction {
	case North, South:
		for i := -1; i <= 1; i++ {
			if t[c.Add(Coord{i, 0})] {
				return false
			}
		}
	case West, East:
		for i := -1; i <= 1; i++ {
			if t[c.Add(Coord{0, i})] {
				return false
			}
		}
	case Stay:
		return true
	}

	return true
}

func (dir Direction) ToCoord() (coord Coord) {
	switch dir {
	case Stay:
		coord = Coord{0, 0}
	case North:
		coord = Coord{0, -1}
	case South:
		coord = Coord{0, 1}
	case West:
		coord = Coord{-1, 0}
	case East:
		coord = Coord{1, 0}
	}
	return
}

func (a Coord) Add(b Coord) (res Coord) {
	res.x = a.x + b.x
	res.y = a.y + b.y
	return
}

func (t Table) String() string {
	min, max := t.Area()
	var sb strings.Builder

	for y := min.y; y <= max.y; y++ {
		for x := min.x; x <= max.x; x++ {
			if _, ok := t[Coord{x, y}]; ok {
				sb.WriteRune('#')
			} else {
				sb.WriteRune('.')
			}
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (t Table) Area() (min, max Coord) {
	isUnset := true
	for coord := range t {
		if isUnset {
			min = coord
			max = coord
			isUnset = false
			continue
		}

		if coord.x < min.x {
			min.x = coord.x
		}
		if coord.x > max.x {
			max.x = coord.x
		}
		if coord.y < min.y {
			min.y = coord.y
		}
		if coord.y > max.y {
			max.y = coord.y
		}
	}
	return
}

func parse(f *os.File) (t Table, err error) {
	t = Table{}
	cont, _ := readContents(f)
	lines := strings.Split(strings.Trim(cont, "\n"), "\n")

	for y, s := range lines {
		for x, r := range s {
			switch r {
			case '#':
				c := Coord{x, y}
				t[c] = true
			case '.':
			default:
				return Table{}, fmt.Errorf("Unexpected rune: '%c'", r)
			}
		}
	}

	return
}

func readContents(f *os.File) (res string, err error) {
	var sb strings.Builder
	b := make([]byte, 1024)
	for {
		n, err := f.Read(b)
		if err != nil {
			if err == io.EOF {
				break
			}
			return sb.String(), err
		}
		sb.Write(b[:n])
	}

	return sb.String(), nil
}

func main() {
	f, err := os.Open("input")
	defer f.Close()

	if err != nil {
		panic(err)
	}
	table, err := parse(f)
	if err != nil {
		panic(err)
	}

	p1, p2 := solve(table)
	println("Part1:", p1)
	println("Part1:", p2)
}
