package aoc

import (
	"os"
)

func readfile(filepath string) (string, error) {
	buf, err := os.ReadFile(filepath)
	if err != nil {
		return "", err
	}

	return string(buf), nil
}
