package aoc

import (
	"fmt"
	"strconv"
	"strings"
)

func runDay1() error {
	inputs, err := readfile("./inputs/day1-puzzle.txt")
	if err != nil {
		return err
	}

	rowStrings := strings.Split(inputs, "\n")

	topThreeHighestCalorie := map[int]int{
		0: 0,
		1: 0,
		2: 0,
	}
	var topThreeSum int

	for rank := range topThreeHighestCalorie {
		var highestCalorie int
		var calorieSum int

		for _, row := range rowStrings {
			if row == "" {
				if calorieSum > highestCalorie {

					if rank > 0 && calorieSum < topThreeHighestCalorie[rank-1] {
						highestCalorie = calorieSum
					}

					if rank == 0 {
						highestCalorie = calorieSum
					}
				}

				calorieSum = 0
			} else {
				calorie, _ := strconv.Atoi(row)
				calorieSum += calorie
			}
		}

		topThreeHighestCalorie[rank] = highestCalorie
		topThreeSum += highestCalorie
	}

	fmt.Println("day 1 result=", topThreeHighestCalorie)
	fmt.Println("day 1 result=", topThreeSum)

	return nil
}
