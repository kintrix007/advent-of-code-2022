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

func part1(t Table) int {
	directionOrder := []Direction{North, South, West, East}

	for i := 0; i < 10; i++ {
		t = iterTable(t, directionOrder)
		directionOrder = append(directionOrder[1:], directionOrder[0])
		// fmt.Println("Round", i+1, "\n"+t.String())
	}

	emptyTileCount := 0
	min, max := t.Area()

	for y := min.y; y <= max.y; y++ {
		for x := min.x; x <= max.x; x++ {
			switch (t[Coord{x, y}]) {
			case true:
			case false:
				emptyTileCount++
			}
		}
	}

	return emptyTileCount
}

func iterTable(t Table, directionOrder []Direction) Table {
	isTargetToCoord := map[Coord]bool{}
	targetToCoord := map[Coord]Coord{}
	isCoordToTarget := map[Coord]bool{}
	coordToTarget := map[Coord]Coord{}

	for coord, v := range t {
		if !v {
			continue
		}
		dir := getMoveDirection(t, coord, directionOrder)
		target := coord.Add(dir.ToCoord())
		if isTargetToCoord[target] {
			from := targetToCoord[target]
			isCoordToTarget[from] = false
			continue
		}
		isTargetToCoord[target] = true
		targetToCoord[target] = coord
		isCoordToTarget[coord] = true
		coordToTarget[coord] = target
	}

	for coord, v := range isCoordToTarget {
		if !v {
			continue
		}
		target := coordToTarget[coord]
		t[coord] = false
		t[target] = true
	}

	return t
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

func (t Table) String() (res string) {
	min, max := t.Area()

	for y := min.y; y <= max.y; y++ {
		for x := min.x; x <= max.x; x++ {
			switch (t[Coord{x, y}]) {
			case true:
				res += "#"
			case false:
				res += "."
			}
		}
		res += "\n"
	}
	return
}

func (t Table) Area() (min, max Coord) {
	isUnset := true
	for coord, v := range t {
		if !v {
			continue
		}
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

func parse(f *os.File) Table {
	set := map[Coord]bool{}
	cont, _ := readContents(f)
	lines := strings.Split(strings.Trim(cont, "\n"), "\n")

	for y, s := range lines {
		for x, r := range s {
			switch r {
			case '#':
				c := Coord{x, y}
				set[c] = true
			case '.':
			default:
				panic(fmt.Sprintf("Unexpected rune: '%c'", r))
			}
		}
	}

	return set
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
		sb.Grow(n)
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

	table := parse(f)
	p1 := part1(table)
	println("Part1:", p1)
}
