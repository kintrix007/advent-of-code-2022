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

	parsed := parse(f)
	fmt.Println(parsed.Area())
	fmt.Println(parsed)
	parsed[Coord{103, 32}] = true
	parsed[Coord{-4, 32}] = true
	fmt.Println(parsed.Area())
	fmt.Println(parsed)
}
