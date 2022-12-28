package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

// Enum Tile
type (
	Tile  int32
	Table [][]Tile
)

const (
	Empty Tile = iota
	Elf
)

func (t Table) String() (s string) {
	for _, row := range t {
		for _, x := range row {
			switch x {
			case Elf:
				s += "#"
			case Empty:
				s += "."
			}
		}
		s += "\n"
	}

	return
}

func parse(f *os.File) (table Table) {
	cont, _ := readContents(f)
	lines := strings.Split(strings.Trim(cont, "\n"), "\n")

	for y, s := range lines {
		table = append(table, make([]Tile, len(s)))
		for x, r := range s {
			switch r {
			case '.':
				table[y][x] = Empty
			case '#':
				table[y][x] = Elf
			default:
				panic(fmt.Sprintf("Unexpected rune: '%c'", r))
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
	fmt.Println(parsed)
}
