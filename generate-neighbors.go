package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

const (
	BoardWidth  int = 32
	BoardHeight int = 30
)

func run() error {
	out, err := os.Create("neighbors.inc")
	if err != nil {
		return err
	}
	defer out.Close()

	//out := io.MultiWriter(file, os.Stdout)

	fmt.Fprintln(out, "NeighborBytes:")

	for y := 0; y < BoardHeight; y++ {
		fmt.Fprintf(out, "\n; row: %d\n", y)
		for x := 0; x < BoardWidth; x++ {

			neighborsX := []int{
				x-1, x, x+1,
				x-1,    x+1,
				x-1, x, x+1,
			}

			neighborsY := []int{
				y-1, y-1, y-1,
				y,        y,
				y+1, y+1, y+1,
			}

			for i := 0; i < 8; i++ {
				if neighborsX[i] < 0 {
					neighborsX[i] = BoardWidth-1
				}
				if neighborsY[i] < 0 {
					neighborsY[i] = BoardHeight-1
				}

				if neighborsX[i] >= BoardWidth {
					neighborsX[i] = 0
				}
				if neighborsY[i] >= BoardHeight {
					neighborsY[i] = 0
				}
			}

			current := (y*(BoardWidth/8)) + (x/8)
			str := make([]string, 8)
			for i := 0; i < 8; i++ {
				str[i] = fmt.Sprintf("%3d", (neighborsY[i]*(BoardWidth/8)) + (neighborsX[i]/8))
			}
			fmt.Fprintf(out, ".byte %s ; current %3d (%2d, %2d)\n", strings.Join(str, ", "), current, x, y)
		}
	}

	return nil
}
