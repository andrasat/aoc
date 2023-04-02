package aoc

import (
	"fmt"
	"strings"
)

func runDay2() error {
	fmt.Println("\n ----- DAY 2 -----")

	scoreMoveMap := map[string]int64{
		"X": 1,
		"Y": 2,
		"Z": 3,
	}

	winMap := map[string]bool{
		"AY": true,
		"BZ": true,
		"CX": true,
	}

	drawMap := map[string]bool{
		"AX": true,
		"BY": true,
		"CZ": true,
	}

	loseMap := map[string]bool{
		"AZ": true,
		"BX": true,
		"CY": true,
	}

	inputs, err := readfile("./inputs/day2-puzzle.txt")
	if err != nil {
		return err
	}

	rowStrings := strings.Split(inputs, "\n")

	var scoreSum int64

	for _, row := range rowStrings {
		result := strings.Replace(row, " ", "", -1)
		ourMove := string(result[1])

		scoreSum += scoreMoveMap[ourMove]

		if winMap[result] {
			scoreSum += 6
		} else if drawMap[result] {
			scoreSum += 3
		} else {
			scoreSum += 0
		}
	}

	fmt.Println("total score: ", scoreSum)

	fmt.Println("\n ----- DAY 2: PART 2 -----")

	moveMap := map[string]string{
		"A": "X",
		"B": "Y",
		"C": "Z",
	}

	var partTwoScoreSum int64

	for _, row := range rowStrings {
		result := strings.Replace(row, " ", "", -1)
		opponentMove := string(result[0])
		ourMove := string(result[1])

		// X means lose
		if ourMove == "X" {
			for loseKey := range loseMap {
				opponentIndex := strings.Index(loseKey, opponentMove)
				if opponentIndex == 0 {
					ourShouldBeMove := string(loseKey[1])
					partTwoScoreSum = partTwoScoreSum + 0 + scoreMoveMap[ourShouldBeMove]
				}
			}
		}

		// Y means draw
		if ourMove == "Y" {
			partTwoScoreSum = partTwoScoreSum + 3 + scoreMoveMap[moveMap[opponentMove]]
		}

		// Z means win
		if ourMove == "Z" {
			for winKey := range winMap {
				opponentIndex := strings.Index(winKey, opponentMove)
				if opponentIndex == 0 {
					ourShouldBeMove := string(winKey[1])
					partTwoScoreSum = partTwoScoreSum + 6 + scoreMoveMap[ourShouldBeMove]
				}
			}
		}
	}

	fmt.Println("total score part two: ", partTwoScoreSum)

	return nil
}
