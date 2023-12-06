package main

import (
	aoc_2022 "github.com/andrasat/aoc/2022"
	aoc_2023 "github.com/andrasat/aoc/2023"
)

func main() {
	err := aoc_2022.Run()
	if err != nil {
		panic(err)
	}

	err = aoc_2023.Run()
	if err != nil {
		panic(err)
	}
}
